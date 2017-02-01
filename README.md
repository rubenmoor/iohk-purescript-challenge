# iohk-purescript-challenge

## Build the server and the bridge

```
cd server
stack build
stack exec generate-ps-exe
```

## Compile the client

Requires purescript compiler version 0.9.3 on your path

```
cd ..
cd client/
npm install
bower install
```

Compile the client and copy `index.html` to server static files dir
```
npm run build
```

## Run the server

```
cd ..
cd server/
stack exec server-exe
```

## Visit the page

Open a browser at <http://localhost:3000/> and click the buttons
