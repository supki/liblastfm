#!/usr/bin/env python
# -*- coding: utf-8 -*-

from fabric.api import local, run, put, env, cd

env.use_ssh_config = True
env.hosts = ["budueba"]


def haddocks():
    local("cabal-dev haddock --hyperlink-source; tar cjf haddocks.tar.gz -C dist/doc/html/ liblastfm")
    with cd("/var/www/budueba.com/htdocs/"):
        put("haddocks.tar.gz", "haddocks.tar.gz")
        run("rm -rf liblastfm; tar xf haddocks.tar.gz")
    local("rm haddocks.tar.gz")


if __name__ == "__main__":
    haddocks()
