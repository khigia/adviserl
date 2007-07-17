#!/bin/sh
 
# ==== Extract path ====
# Extract the path where this file is located (not where it is run
# from), taking care of dereferencing link and simplifying path.
# (code from FHT commented by LCE)
#
# extract initial path
if ( echo "${0}" | grep "^/" >/dev/null 2>&1) ; then
  curpath="${0}"
else
  curpath="`pwd`/${0}"
fi
# locate this file (current one) dereferencing link
curfile=""
while [ "${curpath}" != "/" ] ; do
  if [ -h "${curpath}" ] ; then
    lnk=`ls -l ${curpath} | sed -e s%"^.\\{0,\\}[ ]->[ ]\\{1,\\}"%%`
    if ( echo "${lnk}" | grep "^/" >/dev/null 2>&1 ) ; then
      curpath="${lnk}"
    else
      curpath="`dirname ${curpath}`/${lnk}"
    fi
  else
    curfile="/`basename ${curpath}`${curfile}"
    curpath=`dirname ${curpath}`
  fi
done
# simplify path by transforming "//" in "/"
pattern="/\{2,\}"
while ( echo "${curfile}" | grep "${pattern}" >/dev/null 2>&1 ) ; do
  curfile=`echo "${curfile}" | sed -e s%"${pattern}"%"/"%`
done
# simplify path by removing "./"
pattern="/\./"
while ( echo "${curfile}" | grep "${pattern}" >/dev/null 2>&1 ) ; do
  curfile=`echo "${curfile}" | sed -e s%"${pattern}"%"/"%`
done
# simplify path by transforming "x/y/.." in "x"
pattern="/[^/]\{1,\}/\.\./"
while ( echo "${curfile}" | grep "${pattern}" >/dev/null 2>&1 ) ; do
  curfile=`echo "${curfile}" | sed -e s%"${pattern}"%"/"%`
done
# final path of this file
fileName=`basename ${curfile}`
# directory where is this file
APP_ROOT=`dirname ${curfile}`
# root of the package (assuming this script is in root/bin as expected)
APP_ROOT=`dirname ${APP_ROOT}`

# ==== Configuration ====
APP_NAME="adviserl"
APP_API=adviserl

APP_START_NODE=adv
APP_START_MOD=${APP_API}
APP_START_FUN=start_app
APP_START_ARGS=
APP_STOP_NODE=${APP_START_NODE}_stoppernode
APP_STOP_MOD=${APP_API}
APP_STOP_FUN=stop_node
APP_STOP_ARGS=${APP_START_NODE}

ERL_CMD=`which erl`
ERL_OPT="+W w"
ERL_OPT_PROD=${ERL_OPT}" -smp enable +K true"
ERL_START_CFG="-config ${APP_ROOT}/ebin/env_prod"
ERL_DEBUG_CFG="-config ${APP_ROOT}/ebin/env_dev"

export HEART_COMMAND="$APP_ROOT/$0 start"


# ==== Commands ====
case $1 in
  start)
    echo  "Starting ${APP_NAME} on node ${APP_START_NODE} (detached, heartbeat)"
    ${ERL_CMD} ${ERL_OPT_PROD} ${ERL_START_CFG} \
        -boot start_sasl \
        -sname ${APP_START_NODE} \
        -pa ${APP_ROOT}/ebin \
        -s ${APP_START_MOD} ${APP_START_FUN} ${APP_START_ARGS} \
        -heart -detached
    ;;
 
  debug)
    echo  "Starting ${APP_NAME} in a shell"
    ${ERL_CMD} ${ERL_OPT} ${ERL_DEBUG_CFG} \
        -boot start_sasl \
        -sname ${APP_START_NODE} \
        -pa ${APP_ROOT}/ebin \
        -s ${APP_START_MOD} ${APP_START_FUN} ${APP_START_ARGS}
        #-s toolbar start \
        #-s appmon start \
    ;;
 
  stop)
    echo "Stopping ${APP_NAME}"
    ${ERL_CMD} \
        -noshell \
        -sname ${APP_STOP_NODE} \
        -pa ${APP_ROOT}/ebin \
        -s ${APP_STOP_MOD} ${APP_STOP_FUN} ${APP_STOP_ARGS}
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    echo "  start: start a new node and run application as deamon (heart mode)"
    echo "  debug: start a new shell and run application"
    echo "  stop: stop both application and node"
    exit 1
esac
 
exit 0
                    
