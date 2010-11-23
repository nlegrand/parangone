#!/bin/sh

args=`getopt vhc:n: $*`;

if [ $? -ne 0 ]
then
    usage()
    exit 2
fi

set -- $args

function usage {
    echo "Usage: parangone [-c <cookie>] [-n <node_name>]"
}

function version {
    echo "This is the shell starter of a Parangone node version alpha"
    usage
}

set -- $args
while [ $# -ge 0 ]
do
    case "$1"
        in
	-v)
	    version; exit 0;;
	-h)
	    usage; exit 0;;
        -c)
           cookie="-setcookie $2"; shift; shift;;
	-n)
	    name="-name $2"; shift; shift;;
        --)
            shift; break;;
    esac
done



if [ ! -n "$name" ]
then
    name="-name parangone"
fi

erl -noshell $cookie $name -pa /usr/local/lib/parangone/ -s parangone start -detached