#!/bin/bash

[[ ! -d ~/bin ]] && mkdir ~/bin

#### 2.10.0
# curl -L https://github.com/docker/compose/releases/download/v2.10.0/docker-compose-linux-x86_64 -o # ~/bin/docker-compose-v2
# chmod +x ~/bin/docker-compose-v2
# cp ~/bin/docker-compose-v2 ~/bin/docker-compose

#### 2.23.0
curl -L https://github.com/docker/compose/releases/download/v2.23.0/docker-compose-linux-x86_64 -o ~/bin/docker-compose-v2
chmod +x ~/bin/docker-compose-v2
cp ~/bin/docker-compose-v2 ~/bin/docker-compose

#### END
