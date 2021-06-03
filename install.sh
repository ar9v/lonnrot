#!/usr/bin/env bash
set -euo pipefail


if [ -d ~/bin ]
then
    echo Home bin dir exists... creating symlink...
    ln -s src/cli/compass ~/bin/compass
else
    echo Home bin dir does not exist, will try to create symlink on /usr/bin/
    sudo ln -s src/cli/compas /usr/bin/compass
fi

echo Done
