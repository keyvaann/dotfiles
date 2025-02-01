cd ~/Downloads/wallpaper/
for img in *.png 
do
  export size=`identify -format "%[fx:w]" ${img}`
  if [ $size != "1920" ] 
  then
    convert ${img} -resize 1920x1080 1920_${img}
    echo Resized ${img} to 1920_${img}
  elif [ "1920" != `echo ${img} | cut -d_ -f1` ]
  then
    mv ${img} 1920_${img}
    echo Renamed ${img} 1920_${img}
  fi
done

for img in 1920_*.png 
do
  convert ${img} -resize 1366x768 1366_`echo ${img} | cut -d_ -f2-`
  echo Renamed ${img} 1366_${img}
done
