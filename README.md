# haskell-palette

### dev setup

1. Make sure you have docker installed
2. install [vs code remote containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
3. clone this repo and open it in vs code
4. Open command palette (ctrl+shift+p) and find "Remote Containers: Reopen in container"
5. use cabal cli (eg. cabal run) or ghci to work interactively


### building the demo frontend

Run `./build_demo_frontend.sh`. It will create a index.html in the root directory. By default the frontend is trying to reach the API on http://localhost:3000.

### API

```
POST /upload
{
    "algorithm": "median_cut" | "k_means" | "k_means_pp",
    "image": String (encoded in base64)
}

200 response:
{
    colors: [HexColorString]
}
```