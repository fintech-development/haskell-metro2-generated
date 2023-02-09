#! /bin/bash

# https://openapi-generator.tech/docs/installation
openapi-generator-cli generate \
  -i https://raw.githubusercontent.com/moov-io/metro2/master/api/openapi.yaml \
  -g haskell-http-client \
  -o. \
  --skip-validate-spec \
  --additional-properties=allowToJsonNulls=true \
  --additional-properties=allowFromJsonNulls=true \
  --additional-properties=allowNonUniqueOperationIds=true \
  --additional-properties=ensureUniqueParams=true \
  --additional-properties=useKatip=false \
  --additional-properties=cabalPackage=metro2 \
  --additional-properties=requestType=TheMetro2Request \
  --additional-properties=disallowAdditionalPropertiesIfNotPresent=false
