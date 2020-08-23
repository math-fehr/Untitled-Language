#! /bin/sh


tmpfile=$(mktemp /tmp/ulformat.XXXXXX)

for file in src/*.hs
do
    cat $file | hindent > $tmpfile
    cp $tmpfile $file
done
