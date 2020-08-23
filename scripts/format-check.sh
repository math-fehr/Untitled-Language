#! /bin/sh


tmpfile=$(mktemp /tmp/ulformat.XXXXXX)

for file in src/*.hs
do
    cat $file | hindent --validate > $tmpfile
    if cmp --silent $file $tmpfile;
    then
        true
    else
        echo "$file is not formatted"
        exit 1
    fi
done
