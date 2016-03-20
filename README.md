### 概要

Clownish は Stockfish 7 をベースに将棋依存部分を実装した 64bitOS 用の思考エンジンです。

Linux (xubuntu 14.04 64bit) でのみ動作確認しています。  
いわゆる自分も試しに作ってみました的なノリなので、  
テキトー実装かつ以降の改良とか考えてないです。

### ビルド

srcディレクトリで  
$ make build ARCH=x86-64

binディレクトリに  
実行ファイル clownish が生成されます。

### ライセンス

GPL 3.0

### 謝辞

Stockfish  : Tord Romstad 様、Marco Costalba 様、Joona Kiiski 様、Gary Linscott 様、

実装の参考にさせて頂いた  
NanohaMini : 川端一之 様、  
Usapyon2   : 池泰弘 様、  

に感謝します。
