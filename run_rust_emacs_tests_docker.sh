#!/bin/sh

echo "Testing Local files with Emacs 26 (latest)"
docker run -it --rm --name docker-cp -v `pwd`:/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  silex/emacs:26.2-dev ./test-by-cp

echo Testing Local files with Emacs 25
docker run -it --rm --name docker-cp -v `pwd`:/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  silex/emacs:25.3-dev ./test-by-cp

echo "Testing commits with Emacs 26 (latest)"
docker run -it --rm --name docker-cp -v `pwd`:/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  silex/emacs:26.2-dev ./test-from-git

echo Testing commits with Emacs 25
docker run -it --rm --name docker-cp -v `pwd`:/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  silex/emacs:25.3-dev ./test-from-git


