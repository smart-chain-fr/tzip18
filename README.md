# TEZOS TZIP-018 : A2 - Upgradable Contracts

## Table Of Contents

<!-- TOC -->

- [Table Of Contents](#table-of-contents)
- [Summary](#summary)
- [Motivation](#motivation)
- [Abstract](#abstract)
- [Architecture](#abstract)

<!-- /TOC -->

## Summary

We present here a TZIP-018 working MVP that upgrades a FA12 coin to a FA2 coin in LIGOLANG.
As a bonus feature, this code also provide metadata viewability and an increase of precision x1000 in the decimals while remaining the same supply.

TZIP-018 proposes a set of mechanisms for defining an upgradeable contract with a fixed address. We describe how to define administrator-forced upgrades about the logic and user-defined upgrades about the data. For the sake of simplicity, here a simple address will be the administrator, but of course a DAO governance contract can be plugged instead without any modification.

The proxy contract should be the only one called by the governance, and contains two entrypoints :
1- Call() is for calling any function, it contains the method called as string and a payload as bytes
2- Update() is for updating, and contains the new version contract address, a list of entrypoint operation for adding / modifing / removing "virtual" entrypoints, and the version change

Any version upgrade is a new contract that must be originated first and must contain the bigmaps transition method from the last version. After checking the code, the governance can ask the proxy to Update(), like the following diagram :

### Using the TZIP18 framework

```mermaid
sequenceDiagram
    autonumber
    actor governance
    actor third party
    participant proxy
    participant version 1 (FA 1.2)
    participant version 2 (FA 2.0)
    governance->>proxy: Originate Contract
    governance->>version 1 (FA 1.2): Originate Contract
    governance->>proxy:call_proxy_upgrade(version 1)
    proxy->>version 1 (FA 1.2):fetch_new_metadata()
    proxy->>proxy:Modify new entrypoints
    third party->>version 2 (FA 2.0): Originate Contract
    third party-->>governance: Propose New Version Approval
    governance->>proxy:call_proxy_upgrade(version 2)
    proxy->>version 2 (FA 2.0):fetch new metadata()
    proxy->>proxy:Modify new entrypoints()
    proxy->>version 1 (FA 1.2):put version 2 as master()
```

Using the TZIP-18 proxy is easy and straighforward :

### Calling a TZIP18 contract

```mermaid
sequenceDiagram
    autonumber
    actor third party
    participant proxy
    participant version current
    participant version old
    third party->>proxy: call_contract()
    proxy->>version current: Forward Call
    version current->>version old: (optional) update map 
    version current->>version current: Execute Logic 
```

Note that the version contrats can never be called from outside.

### Caracteristics :

The design supports :
- adding/removing entrypoints
- changing an entrypoint's code and parameter type
- changing the storage's content and type
- contract address immutability

## Motivation

This proposal provides the TZIP-18 properties :

1. Security : During the past several years, bugs and vulnerabilities in smart contracts caused millions of dollars to get stolen or lost forever. Such cases may even require manual intervention in blockchain operation to recover the funds. As a result, the community starts to acknowledge the need for upgradeable smart-contracts.

2. Adding features : Having logic fixed at origination makes the contract somehow wobbly. To unleach the true potential of Tezos framework, this is an important question to answer.

3. Version management : TZIP-18 automaticaly manages differents versions and contract interconnections.

4. Fixed entrypoints : Callees like light clients, wallet and explorer can call a same entrypoint again and again, totaly separating the logic from the address.

5. Hot fixes : This proposal is convenient for fixing critical bugs or when a vulnerability is discovered in the contract's implementation, since a new version can quickly be released.

## Abstract

Implementing an upgradeable contract is not a trivial task, there are many considerations to address, such as address immutability and trust between users and administrators.

Here we present a contract capable of being upgraded by an administrator, without the users' consent, whilst retaining the existing contract's address. We call these "administrator-forced upgrades". The user will call the same proxy contract, and will be automaticaly lazily upgraded.

Thanks to address immutability, this mechanism facilitates pushing upgrades to all users without requiring them to update the contract's address in their wallets.

Administrator-forced upgrades are not applicable to some use-cases because they require a certain degree of trust in the person or organization that manages the contract. In that particular case, people using the token must vote on the contract upgrade, or the architecture presented here must be slightly modified. This proposal does not cover this case. The administration problem is in fact totaly independant from the TZIP18 proposal, so in this POC we will only present a basic governance of only one user. Of course in a real project, a governance smart-contract must be created.

