{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import METRO2.Model
import METRO2.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary BaseSegment where
  arbitrary = sized genBaseSegment

genBaseSegment :: Int -> Gen BaseSegment
genBaseSegment n =
  BaseSegment
    <$> arbitraryReducedMaybe n -- baseSegmentBlockDescriptorWord :: Maybe Int
    <*> arbitrary -- baseSegmentRecordDescriptorWord :: Int
    <*> arbitraryReducedMaybe n -- baseSegmentTimeStamp :: Maybe DateTime
    <*> arbitrary -- baseSegmentIdentificationNumber :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentCycleIdentifier :: Maybe Text
    <*> arbitrary -- baseSegmentConsumerAccountNumber :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentPortfolioType :: Maybe E'PortfolioType
    <*> arbitrary -- baseSegmentAccountType :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentDateOpened :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- baseSegmentCreditLimit :: Maybe Int
    <*> arbitrary -- baseSegmentHighestCredit :: Int
    <*> arbitrary -- baseSegmentTermsDuration :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentTermsFrequency :: Maybe E'TermsFrequency
    <*> arbitraryReducedMaybe n -- baseSegmentScheduledMonthlyPaymentAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseSegmentActualPaymentAmount :: Maybe Int
    <*> arbitrary -- baseSegmentAccountStatus :: E'AccountStatus
    <*> arbitraryReducedMaybe n -- baseSegmentPaymentRating :: Maybe E'PaymentRating
    <*> arbitrary -- baseSegmentPaymentHistoryProfile :: E'PaymentHistoryProfile
    <*> arbitraryReducedMaybe n -- baseSegmentSpecialComment :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseSegmentComplianceConditionCode :: Maybe Text
    <*> arbitrary -- baseSegmentCurrentBalance :: Int
    <*> arbitraryReducedMaybe n -- baseSegmentAmountPastDue :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseSegmentOriginalChargeOffAmount :: Maybe Int
    <*> arbitraryReduced n -- baseSegmentDateAccountInformation :: DateTime
    <*> arbitraryReducedMaybe n -- baseSegmentDateFirstDelinquency :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- baseSegmentDateClosed :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- baseSegmentDateLastPayment :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- baseSegmentInterestTypeIndicator :: Maybe Text
    <*> arbitrary -- baseSegmentSurname :: Text
    <*> arbitrary -- baseSegmentFirstName :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseSegmentGenerationCode :: Maybe E'GenerationCode
    <*> arbitrary -- baseSegmentSocialSecurityNumber :: Int
    <*> arbitraryReduced n -- baseSegmentDateBirth :: DateTime
    <*> arbitraryReducedMaybe n -- baseSegmentTelephoneNumber :: Maybe Integer
    <*> arbitrary -- baseSegmentEcoaCode :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentConsumerInformationIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseSegmentCountryCode :: Maybe Text
    <*> arbitrary -- baseSegmentFirstLineAddress :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentSecondLineAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseSegmentCity :: Maybe Text
    <*> arbitrary -- baseSegmentState :: Text
    <*> arbitrary -- baseSegmentZipCode :: Text
    <*> arbitraryReducedMaybe n -- baseSegmentAddressIndicator :: Maybe E'AddressIndicator
    <*> arbitraryReducedMaybe n -- baseSegmentResidenceCode :: Maybe E'ResidenceCode
  
instance Arbitrary DataRecord where
  arbitrary = sized genDataRecord

genDataRecord :: Int -> Gen DataRecord
genDataRecord n =
  DataRecord
    <$> arbitraryReduced n -- dataRecordBase :: BaseSegment
    <*> arbitraryReducedMaybe n -- dataRecordJ1 :: Maybe [J1Segment]
    <*> arbitraryReducedMaybe n -- dataRecordJ2 :: Maybe [J2Segment]
    <*> arbitraryReducedMaybe n -- dataRecordK1 :: Maybe K1Segment
    <*> arbitraryReducedMaybe n -- dataRecordK2 :: Maybe K2Segment
    <*> arbitraryReducedMaybe n -- dataRecordK3 :: Maybe K3Segment
    <*> arbitraryReducedMaybe n -- dataRecordK4 :: Maybe K4Segment
    <*> arbitraryReducedMaybe n -- dataRecordL1 :: Maybe L1Segment
    <*> arbitraryReducedMaybe n -- dataRecordN1 :: Maybe N1Segment
  
instance Arbitrary File where
  arbitrary = sized genFile

genFile :: Int -> Gen File
genFile n =
  File
    <$> arbitraryReduced n -- fileHeader :: HeaderRecord
    <*> arbitraryReducedMaybe n -- fileData :: Maybe [DataRecord]
    <*> arbitraryReduced n -- fileTrailer :: TrailerRecord
  
instance Arbitrary HeaderRecord where
  arbitrary = sized genHeaderRecord

genHeaderRecord :: Int -> Gen HeaderRecord
genHeaderRecord n =
  HeaderRecord
    <$> arbitraryReducedMaybe n -- headerRecordBlockDescriptorWord :: Maybe Int
    <*> arbitrary -- headerRecordRecordDescriptorWord :: Int
    <*> arbitrary -- headerRecordRecordIdentifier :: E'RecordIdentifier
    <*> arbitraryReducedMaybe n -- headerRecordCycleIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- headerRecordInnovisProgramIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- headerRecordEquifaxProgramIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- headerRecordExperianProgramIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- headerRecordTransUnionProgramIdentifier :: Maybe Text
    <*> arbitraryReduced n -- headerRecordActivityDate :: DateTime
    <*> arbitraryReduced n -- headerRecordDateCreated :: DateTime
    <*> arbitraryReducedMaybe n -- headerRecordProgramDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- headerRecordProgramRevisionDate :: Maybe DateTime
    <*> arbitrary -- headerRecordReporterName :: Text
    <*> arbitrary -- headerRecordReporterAddress :: Text
    <*> arbitraryReducedMaybe n -- headerRecordReporterTelephoneNumber :: Maybe Integer
    <*> arbitraryReducedMaybe n -- headerRecordSoftwareVendorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- headerRecordSoftwareVersionNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- headerRecordPrbcProgramIdentifier :: Maybe Text
  
instance Arbitrary J1Segment where
  arbitrary = sized genJ1Segment

genJ1Segment :: Int -> Gen J1Segment
genJ1Segment n =
  J1Segment
    <$> arbitrary -- j1SegmentSegmentIdentifier :: E'SegmentIdentifier
    <*> arbitrary -- j1SegmentSurname :: Text
    <*> arbitrary -- j1SegmentFirstName :: Text
    <*> arbitraryReducedMaybe n -- j1SegmentMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- j1SegmentGenerationCode :: Maybe E'GenerationCode2
    <*> arbitrary -- j1SegmentSocialSecurityNumber :: Int
    <*> arbitraryReduced n -- j1SegmentDateBirth :: DateTime
    <*> arbitraryReducedMaybe n -- j1SegmentTelephoneNumber :: Maybe Integer
    <*> arbitrary -- j1SegmentEcoaCode :: Text
    <*> arbitraryReducedMaybe n -- j1SegmentConsumerInformationIndicator :: Maybe Text
  
instance Arbitrary J2Segment where
  arbitrary = sized genJ2Segment

genJ2Segment :: Int -> Gen J2Segment
genJ2Segment n =
  J2Segment
    <$> arbitrary -- j2SegmentSegmentIdentifier :: E'SegmentIdentifier2
    <*> arbitrary -- j2SegmentSurname :: Text
    <*> arbitrary -- j2SegmentFirstName :: Text
    <*> arbitraryReducedMaybe n -- j2SegmentMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- j2SegmentGenerationCode :: Maybe E'GenerationCode2
    <*> arbitrary -- j2SegmentSocialSecurityNumber :: Int
    <*> arbitraryReduced n -- j2SegmentDateBirth :: DateTime
    <*> arbitraryReducedMaybe n -- j2SegmentTelephoneNumber :: Maybe Integer
    <*> arbitrary -- j2SegmentEcoaCode :: Text
    <*> arbitraryReducedMaybe n -- j2SegmentConsumerInformationIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- j2SegmentCountryCode :: Maybe Text
    <*> arbitrary -- j2SegmentFirstLineAddress :: Text
    <*> arbitraryReducedMaybe n -- j2SegmentSecondLineAddress :: Maybe Text
    <*> arbitrary -- j2SegmentCity :: Text
    <*> arbitrary -- j2SegmentState :: Text
    <*> arbitrary -- j2SegmentZipCode :: Text
    <*> arbitraryReducedMaybe n -- j2SegmentAddressIndicator :: Maybe E'AddressIndicator
    <*> arbitraryReducedMaybe n -- j2SegmentResidenceCode :: Maybe E'ResidenceCode
  
instance Arbitrary K1Segment where
  arbitrary = sized genK1Segment

genK1Segment :: Int -> Gen K1Segment
genK1Segment n =
  K1Segment
    <$> arbitrary -- k1SegmentSegmentIdentifier :: E'SegmentIdentifier3
    <*> arbitrary -- k1SegmentOriginalCreditorName :: Text
    <*> arbitrary -- k1SegmentCreditorClassification :: E'CreditorClassification
  
instance Arbitrary K2Segment where
  arbitrary = sized genK2Segment

genK2Segment :: Int -> Gen K2Segment
genK2Segment n =
  K2Segment
    <$> arbitrary -- k2SegmentSegmentIdentifier :: E'SegmentIdentifier4
    <*> arbitrary -- k2SegmentPurchasedIndicator :: E'PurchasedIndicator
    <*> arbitrary -- k2SegmentPurchasedName :: Text
  
instance Arbitrary K3Segment where
  arbitrary = sized genK3Segment

genK3Segment :: Int -> Gen K3Segment
genK3Segment n =
  K3Segment
    <$> arbitrary -- k3SegmentSegmentIdentifier :: E'SegmentIdentifier5
    <*> arbitraryReducedMaybe n -- k3SegmentAgencyIdentifier :: Maybe E'AgencyIdentifier
    <*> arbitraryReducedMaybe n -- k3SegmentAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- k3SegmentMortgageIdentificationNumber :: Maybe Text
  
instance Arbitrary K4Segment where
  arbitrary = sized genK4Segment

genK4Segment :: Int -> Gen K4Segment
genK4Segment n =
  K4Segment
    <$> arbitrary -- k4SegmentSegmentIdentifier :: E'SegmentIdentifier6
    <*> arbitrary -- k4SegmentSpecializedPaymentIndicator :: E'SpecializedPaymentIndicator
    <*> arbitraryReducedMaybe n -- k4SegmentDeferredPaymentStartDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- k4SegmentBalloonPaymentDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- k4SegmentBalloonPaymentAmount :: Maybe Int
  
instance Arbitrary L1Segment where
  arbitrary = sized genL1Segment

genL1Segment :: Int -> Gen L1Segment
genL1Segment n =
  L1Segment
    <$> arbitrary -- l1SegmentSegmentIdentifier :: E'SegmentIdentifier7
    <*> arbitrary -- l1SegmentChangeIndicator :: Int
    <*> arbitraryReducedMaybe n -- l1SegmentNewConsumerAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- l1SegmentBalloonPaymentDueDate :: Maybe Text
  
instance Arbitrary N1Segment where
  arbitrary = sized genN1Segment

genN1Segment :: Int -> Gen N1Segment
genN1Segment n =
  N1Segment
    <$> arbitrary -- n1SegmentSegmentIdentifier :: E'SegmentIdentifier8
    <*> arbitrary -- n1SegmentEmployerName :: Text
    <*> arbitraryReducedMaybe n -- n1SegmentFirstLineEmployerAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- n1SegmentSecondLineEmployerAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- n1SegmentEmployerCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- n1SegmentEmployerState :: Maybe Text
    <*> arbitraryReducedMaybe n -- n1SegmentZipCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- n1SegmentOccupation :: Maybe Text
  
instance Arbitrary TrailerRecord where
  arbitrary = sized genTrailerRecord

genTrailerRecord :: Int -> Gen TrailerRecord
genTrailerRecord n =
  TrailerRecord
    <$> arbitraryReducedMaybe n -- trailerRecordBlockDescriptorWord :: Maybe Int
    <*> arbitrary -- trailerRecordRecordDescriptorWord :: Int
    <*> arbitrary -- trailerRecordRecordIdentifier :: E'RecordIdentifier2
    <*> arbitraryReducedMaybe n -- trailerRecordTotalBaseRecords :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCodeDf :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalConsumerSegmentsJ1 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalConsumerSegmentsJ2 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordBlockCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCodeDa :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode05 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode11 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode13 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode61 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode62 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode63 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode64 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode65 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode71 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode78 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode80 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode82 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode83 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode84 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode88 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode89 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode93 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode94 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode95 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode96 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalStatusCode97 :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalEcoaCodeZ :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalEmploymentSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalOriginalCreditorSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalPurchasedToSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalMortgageInformationSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalPaymentInformationSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalChangeSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalSocialNumbersAllSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalSocialNumbersBaseSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalSocialNumbersJ1Segments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalSocialNumbersJ2Segments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalDatesBirthAllSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalDatesBirthBaseSegments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalDatesBirthJ1Segments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalDatesBirthJ2Segments :: Maybe Int
    <*> arbitraryReducedMaybe n -- trailerRecordTotalTelephoneNumbersAllSegments :: Maybe Int
  



instance Arbitrary E'AccountStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AddressIndicator where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AgencyIdentifier where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CreditorClassification where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Format where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'GenerationCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'GenerationCode2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PaymentHistoryProfile where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PaymentRating where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PortfolioType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PurchasedIndicator where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RecordIdentifier where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RecordIdentifier2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResidenceCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier6 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier7 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SegmentIdentifier8 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SpecializedPaymentIndicator where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'TermsFrequency where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

