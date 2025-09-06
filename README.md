# 🌞 Solchain - Community Solar Funding DAO

A decentralized platform for funding community solar projects through fractional investments on the Stacks blockchain.

## ⚡ Features

- Create solar funding projects
- Invest in projects using STX
- Receive SOLTOKEN as proof of investment
- Automatic project finalization after funding period
- Refund mechanism for failed projects

## 🚀 Usage

### Creating a Project

```clarity
(contract-call? .solchain create-project u1000000000)
```

### Investing in a Project

```clarity
(contract-call? .solchain invest u1 u5000000)
```

### Finalizing a Project

```clarity
(contract-call? .solchain finalize-project u1)
```

### Claiming Refund (if project fails)

```clarity
(contract-call? .solchain claim-refund u1)
```

## 📊 Read-Only Functions

- `get-project`: View project details
- `get-investment`: Check investment amount
- `get-total-projects`: Get number of projects
- `get-total-funds`: View total funds raised

## 🔒 Security

- Minimum investment threshold
- Maximum project cap
- Owner-only finalization
- Automated status updates
- Secure fund management

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📜 License

MIT

