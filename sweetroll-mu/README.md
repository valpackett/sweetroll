# Sweetroll Media Upload

The media upload service implements the [Micropub Media Endpoint](https://www.w3.org/TR/micropub/#media-endpoint) spec, extended to return a JSON object in addition to the `Location` header, which allows Sweetroll to store image metadata extracted from Exif sections and store links to different formats of images/videos (e.g. WebP + JPEG, WebM + MP4).
micro-panel simply picks up the JSON body if present instead of the `Location` header, shoves it into the `photo`/`video`/etc. property, and the templates in sweetroll-fe can work with that JSON object.

This service currently does image optimization/transcoding and Exif metadata extraction.
TODO: video.

This service can work locally with local uploads (`index.js`) and on AWS Lambda with AWS S3 uploads (`lambda.js`).

## Development

You need a recent version (at least 7.6.0) of [Node.js].

Use [yarn] to build.

```bash
$ yarn
```

[Node.js]: https://nodejs.org/en/
[yarn]: https://yarnpkg.com/en/

## Amazon Lambda Deployment

First, set up the [AWS CLI](https://aws.amazon.com/cli/) on your machine, use `aws configure` to log in.

To build the Lambda image, in particular to get native dependencies for Linux, you need Linux, preferably the same Linux as on Lambda (though so far most native dependencies simply fetch a Linux binary instead of building from source).
Using the same Linux is easy with AWS CodeBuild.
It's actually a pretty cool service!

So, set up a CodeBuild project called `sweetroll-mu` with

- `aws/codebuild/eb-nodejs-6.10.0-amazonlinux-64:4.0.0` as the image;
- `sweetroll-mu-src.zip` in your S3 bucket as the source;
- `sweetroll-mu-dist.zip` (also in S3) as the artifact.
  - Don't forget to select Artifacts packaging = Zip in Advanced settings!

Now you can do this:

```bash
$ S3_BUCKET=mybucketname ./buildlambda.sh
```

And after about a minute the `…-dist.zip` will appear in S3.

Then set up a Lambda function with an API Gateway trigger.
Prepare yourself, the latter is kinda terrible, especially with the web admin console :D

So, for the Lambda itself:

- Runtime `Node.js 6.10`, Handler `lambda.handler`
- environment variables:
  - `S3_BUCKET` for the uploads e.g. `unrelentingtech`
  - `S3_URL` for that bucket e.g. `https://unrelentingtech.s3.dualstack.eu-west-1.amazonaws.com/`
  - `SWEETROLL_SECRET` the same JWT secret as in the other services
    - Encrypt it here with a KMS key!

The policy for the lambda's role should look like this:

```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "logs:CreateLogGroup",
                "logs:CreateLogStream",
                "logs:PutLogEvents"
            ],
            "Resource": "arn:aws:logs:*:*:*"
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:PutObject",
                "s3:PutObjectAcl"
            ],
            "Resource": [
                "arn:aws:s3:::unrelentingtech/*"
            ]
        }
    ]
}
```

(Basically the default one plus S3 uploads. Note the `PutObjectAcl`! Can't upload public files without that.)

Also go to the KMS key you've created (IAM → Encryption keys), make sure the role is added to Key Users!

Now, the API gateway.

Go to Actions → Enable CORS.
That'll create an OPTIONS handler with CORS headers, but won't add them to the ANY handler that calls the lambda.

Now go to the ANY handler → Integration Request.
Uncheck "Use Lambda Proxy integration", open "Body Mapping Templates", set "Request body passthrough" to "Never", create the following mapping template for `multipart/form-data`:

```velocity
#set($inputParams = $input.body)
#set($contentType = $input.params().header.get("Content-Type"))
#set($auth = $input.params().header.get("Authorization"))
{
  "body": "$inputParams",
  "contentType": "$contentType",
  "authorization": "$auth"
}
```

Don't forget to "Save".
Yes, the multipart body will be shoved into the JSON as Base64, it's horrible, I think they kinda promised to add normal body support eventually.

Now go to "Method Response".
Remove the 200, create a 201, add the following headers:

- `Access-Control-Allow-Headers`
- `Access-Control-Allow-Origin`
- `Access-Control-Allow-Methods`
- `Location`

Don't worry about "No models" in the body section, go to "Integration Response", remove the 200 if it's there, add one for 201.

Header mappings:
- `Access-Control-Allow-Headers`	`'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'`
- `Access-Control-Allow-Origin`	`'*'`
- `Access-Control-Allow-Methods`	`'DELETE,GET,HEAD,OPTIONS,PATCH,POST,PUT'`
- `Location`	`integration.response.body.location` (without quotes! That's an expression that'll take the value from the lambda's response)

Body Mapping Templates: one for `application/json`:

```velocity
$input.json('$.body')
```

Deploy to prod.
Should work now :D

## General project info

See `../README.md`.
