#!/bin/bash - 
#===============================================================================
#
#          FILE: cpan.sh
# 
#         USAGE: ./cpan.sh
# 
#   DESCRIPTION: cpan Modules Download script
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic
#  ORGANIZATION: 
#       CREATED: 2021-09-13
#      REVISION: ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
set -e
set -u
set -o pipefail
IFS=$'\n\t'

export PATH=/opt/cperl5/bin:/opt/cperl5/cpan/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:

cpan CGI
cpan Curses
cpan Cwd
# cpan Data
cpan Sub::Identify
cpan DateTime
cpan DateTime::Format::LDAP;
cpan DBI
# cpan Device
cpan Digest
cpan Encode
cpan Expect
cpan Fcntl
# cpan File
# cpan Getopt
# cpan HTTP
cpan IO
cpan IO::Prompter
# cpan IPC
cpan JSON
cpan lib
# cpan Linux
cpan local
# cpan Logit
cpan LWP
cpan Net::LDAP
cpan Net::LDAP::Filter
# cpan MIME
# cpan Net
cpan Net::LDAP
cpan Net::Telnet
cpan OLE
cpan PAR
cpan PAR::Packer
cpan POSIX
cpan REST
# cpan Scalar
# cpan SimpleCluster
cpan Socket
cpan Spreadsheet::WriteExcel
cpan Spreadsheet::ParseExcel
# cpan StatisticEvents
# cpan String
cpan Switch
# cpan Sys
# cpan Term
cpan Term::ReadKey
# cpan Text
cpan Thread
cpan threads
# cpan Time
# cpan Trace
# cpan UI
cpan utf8
cpan utf8:all
# cpan utils
cpan vars
cpan XML::Parser
# cpan WWW

## END
