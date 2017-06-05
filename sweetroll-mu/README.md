# Sweetroll Media Upload

The media upload service implements the [Micropub Media Endpoint](https://www.w3.org/TR/micropub/#media-endpoint) spec, extended to return a JSON object in addition to the `Location` header, which allows Sweetroll to store image metadata extracted from Exif/XMP/etc. sections and store links to different formats of images/videos (e.g. WebP + JPEG, WebM + MP4).
micro-panel simply picks up the JSON body if present instead of the `Location` header, shoves it into the `photo`/`video`/etc. property, and the templates in sweetroll-fe can work with that JSON object.

This service currently does image optimization/transcoding and metadata extraction.
TODO: video.

This service can work locally (`index.js`) and on AWS Lambda (`lambda.js`).

Supports local filesystem and Amazon S3 backends.

## Development

You need a recent version (at least 7.6.0) of [Node.js].

Use npm to build.

```bash
$ npm i
```

[Node.js]: https://nodejs.org/en/

## Regular Server Deployment

Environment variables:

- `SWEETROLL_SECRET`: same value as backend and frontend
- `UPLOAD_BACKEND`: `S3` or `fs`
- With fs backend:
	- `FS_ROOT`: where to put files
	- `FS_URL`: where files from that root will be served
- With S3 backend:
	- `S3_BUCKET`: bucket name
	- `S3_URL`: where files from that bucket will be served (e.g. `https://unrelentingtech.s3.dualstack.eu-west-1.amazonaws.com/`, note the `dualstack` URL for IPv6 support)
	- provide AWS credentials the usual way (the SDK handles it), e.g. `awscli` creates `~/.aws/credentials` and that works here

```bash
$ node index.js --port 3333 # 3333 is default
$ node index.js --protocol activate # socket activation (listen on file descriptor 3)
$ node index.js --protocol unix --socket /var/run/sweetroll-mu/sweetroll-mu.sock # unix domain socket
```

## Amazon Lambda Deployment

**NOTE**: Lambda has a terrible body size limit! (6 MB minus base64 overhead) Avoid it for now unless you'll never upload large media files (about 4 MB an up).
Also currently it's broken because exiv2 is not installed in the build.

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

Go to Resources → Actions → Enable CORS.
That'll create an OPTIONS handler with CORS headers.

Go to Binary Support, add multipart/form-data.

Redeploy to prod (Resources → Actions → Deploy API).
Should work now :D

## General project info

See `../README.md`.
