
//2020年9月1日更新
Shinytest : ディレクトリフォルダ
IMG_0444 : 参考動画（Mac,iphonだと標準で観られる、Windowsの場合メディアプレイヤーが最新版でしか観られない）
testData.csv : 動作確認用のtestData


事前の準備
・RstudioからのRの起動が必須となっているので、Rstudioをインストールする必要があります。

	Rstudioのダウンロード元 : https://www.rstudio.com/products/rstudio/download/
	インストール方法参考URL : https://qiita.com/hujuu/items/ddd66ae8e6f3f989f2c0

・また以下の必要パッケージをR経由で入れておきます
	install.packages("shiny")
	install.packages("graph")
	install.packages("DT")
	install.packages("shinyjs")
	install.packages("colourpicker")
	install.packages("rhandsontable")


起動方法
・RstudioからワーキングディレクトリをShinytestフォルダに設定する
・Shinytest内のスクリプト server.Rとui.RをRstudioにて開く

####以下は添付動画IMG_0444でも確認できる####
Shinyパッケージが正しくインストールされている状態で、
Server.R又はui.Rを開くと右上にRun.APPというボタンが現れるので、それをクリックすると、プログラムが起動する


起動後の使用方法
データ入力　：最初の画面で表示される右側サンプルと同様の構造のcsvファイルをドラッグ&ドロップすると、データを読み込む
初期値の入力　：初期値は0で統一されているが、初期値入力タブで変更可能。
可視化　：可視化された結果は可視化タブにて確認できる。見づらい場合は曲率を変更したり、残高を消したりして対応する
詳細設定　：その他詳細な設定を行いたい場合に利用する

懸念事項
原因不明だが、初期値入力タブを一度経由してからデータの読み込みを行わないと、何故かエラーを吐いて落ちる。
そのため、若干不自然だが、起動時に表示されるタブをデータ入力ではなく初期値タブにしている。

