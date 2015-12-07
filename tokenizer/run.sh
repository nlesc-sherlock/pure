#!/bin/sh
cat $@ | docker run -i --rm --name tolerant_tokenizer wrvhage/pure-tokenizer
