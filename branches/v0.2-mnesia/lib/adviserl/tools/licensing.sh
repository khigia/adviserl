LICENSE_FIRST_LINE=`head -n 1 LICENSE_NOTICE_ERL.txt`

FILES=`find .. \( -name "*.erl" -o -name "*.hrl" -o -name Emakefile \) -print`
for i in $FILES ; do
  FirstLine=`head -n 1 $i`
  if [[ $FirstLine != $LICENSE_FIRST_LINE ]] ; then
    echo "Licensing "$i
    cat $i > $i.before_license.bak
    cat NOTICE > $i.tmp
    cat $i >> $i.tmp
    \mv $i.tmp $i
  fi
done

