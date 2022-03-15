# tzip18
### *Contract upgradability in LIGO!*

##### I. Installation

`npm i @taquito/taquito`
`npm i @taquito/signer`
`npm i dotenv`
`npm i nvm`

## Summary

##### II. Compilation of the FA12 Token
- At root, with docker run :
`docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next compile contract contract/fa12.mligo > contract/compiled/fa12.tz`

##### III. Deployment of the FA12 Token
- At root, with docker run :
`docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next compile contract contract/fa12.mligo --michelson-format json > deploy/fa12.json`
- In the folder /deploy, run :
`tsc deploy/deploy.ts --resolveJsonModule -esModuleInterop`
- And then when the deploy.js file is created, run :
`node deploy/deploy.js`
