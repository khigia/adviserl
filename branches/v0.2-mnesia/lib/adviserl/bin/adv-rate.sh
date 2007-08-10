LOCAL_NODE=adv_cmd@localhost
API_NODE=adv@localhost

help_and_exit() {
    echo "Terminating... $1" >&2
    echo "Usage: $0 [-n api_erlang_node] [-s command_erlang_node] SourceID ItemID RatingInteger"
    exit 1
}

# parse options
TEMP=`getopt -o n:s: -n '$0' -- "$@"`
if [ $? != 0 ] ; then help_and_exit "error while parsing options" ; fi
eval set -- "$TEMP"
while true ; do
    case "$1" in
        -n)
            API_NODE="$2" ;
            shift 2 ;;
        -s)
            LOCAL_NODE="$2" ;
            shift 2 ;;
        --)
            shift ;
            break ;;
        *)
            help_and_exit "internal error while parsing options" ;;
    esac
done

# command line remaining arguments
if [ $# != 3 ] ; then help_and_exit "error while parsing arguments" ; fi
SOURCE=$1
ITEM=$2
RATING=$3

# run an Erlang node connecting to the (hopefully running) adviserl node
erl \
    -noshell \
    -sname $LOCAL_NODE \
    -eval "\
        case catch(gen_server:call({adv_api,$API_NODE}, {rate, $SOURCE, $ITEM, {$RATING,nodata}})) of\
            ok -> io:format(\"ok~n\", []);\
            {'EXIT', Any} -> io:format(\"failed: ~w~n\", [Any])
        end.\
    " \
    -s erlang halt
