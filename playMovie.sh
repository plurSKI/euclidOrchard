mplayer --help &> /dev/null
if [ "$?" -ne 127 ]
then
  mplayer -fs $1
  exit
fi

vlc --help &> /dev/null
if [ "$?" -ne 127 ]
then
  vlc --fullscreen $1
  exit
fi

xine --help &> /dev/null
if [ "$?" -ne 127 ]
then
  xine -f $1
  exit
fi

totem --help &> /dev/null
if [ "$?" -ne 127 ]
then
  totem --fullscreen $1
  exit
fi
