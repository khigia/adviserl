export PATH=`dirname $0`:$PATH
PWD_IS_TRUNK=`test -e .svn/entries && cat .svn/entries | egrep url | egrep 'trunk"$'`
TRUNK_IS_MODIFIED="`svn status $TRUNK | egrep -v '^\?|^X|^\s+S\s|^Performing status on external|^$'`"
BRANCH_IS_MODIFIED="`svn status $BRANCH | egrep -v '^\?|^X|^\s+S\s|^Performing status on external|^$'`"
