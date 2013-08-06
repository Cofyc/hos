#!/bin/sh
bench='ab'
#bench='cb'
$bench -n 1000 -c 100 http://studio.dev/index.html
$bench -n 1000 -c 100 http://yechengfu.dev:8080/
curl -i http://yechengfu.dev:8080/test.php
