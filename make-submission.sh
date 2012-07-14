#!/bin/sh

ID=96342042
SUBMISSIONFILE=icfp-$ID.tgz
TEMPSUBMISSION=/tmp/submission

rm 
rm -rf $TEMPSUBMISSION
mkdir $TEMPSUBMISSION
mkdir $TEMPSUBMISSION/src/

cp submission/* $TEMPSUBMISSION/
cp *.asd *.lisp $TEMPSUBMISSION/src/
buildapp --load-system lambda-lifter --entry lambda-lifter:main --output $TEMPSUBMISSION/lifter
tar -czf icfp-$ID.tgz -C $TEMPSUBMISSION .

