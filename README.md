# TZIP-18

## Summary

Contract upgradability in LIGO!

This POC was done from the work of Derek Sorensen, from the original post https://hackmd.io/@durlicc/r16q6D1Zq



## Installation

`npm i @taquito/taquito`
`npm i @taquito/signer`
`npm i dotenv`
`npm i nvm`

## Compilation of the FA12 and FA2 Token
- At root, with docker run :
`docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next compile contract contract/fa12.mligo > contract/compiled/fa12.tz`
`docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next compile contract contract/fa2.mligo > contract/compiled/fa2.tz`

## Deployment of the FA12 Token
- At root, with docker run :
`docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next compile contract contract/fa12.mligo --michelson-format json > deploy/fa12.json`
`docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next compile contract contract/fa2.mligo --michelson-format json > deploy/fa2.json`
- In the folder /deploy, run :
`tsc deploy/deploy.ts --resolveJsonModule -esModuleInterop`
- And then when the deploy.js file is created, run :
`node deploy/deploy.js`
