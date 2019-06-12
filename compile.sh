#npm install

rmdir ./dist
mkdir ./dist
cp ./src/index.html ./dist

#./node_modules/elm-format/unpacked_bin/elm-format.exe src/ --yes
./node_modules/elm/binwrappers/elm make src/Main.elm --output=./dist/elm.js --yes
