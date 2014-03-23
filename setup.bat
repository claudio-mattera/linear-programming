@echo off

echo Initializing sandbox
cabal sandbox init

echo Installing dependencies
cabal install --only-dependencies

echo Configuring Project
cabal configure --enable-tests
