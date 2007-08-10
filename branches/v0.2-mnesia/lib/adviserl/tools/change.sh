
FILES=`find . \( -name "*.erl" -o -name "*.hrl" -o -name Makefile \) -print`

for i in $FILES ; do
  echo "Changing "$i
  cat $i | \
    sed -e "s/sln_/adv_/g" \
    > $i.tmp
  mv -v $i.tmp $i
done

