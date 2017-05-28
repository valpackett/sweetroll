#!/bin/sh
set -e
: ${CB_PROJECT:="sweetroll-mu"}
zip -r sweetroll-mu-src.zip buildspec.yml package.json package-lock.json lib lambda.js
aws s3 cp sweetroll-mu-src.zip s3://$S3_BUCKET/ --storage-class REDUCED_REDUNDANCY
aws codebuild start-build --project-name $CB_PROJECT
rm sweetroll-mu-src.zip
echo "Build started on CodeBuild."
