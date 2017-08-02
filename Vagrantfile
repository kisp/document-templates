# -*- mode: ruby -*-

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/trusty64"

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get -y install build-essential automake libcurl4-openssl-dev
    su - vagrant -c 'curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh'
    su - vagrant -c 'ros install qlot'
    su - vagrant -c "echo 'PATH=~/.roswell/bin:$PATH' >>~/.profile"
  SHELL
end
