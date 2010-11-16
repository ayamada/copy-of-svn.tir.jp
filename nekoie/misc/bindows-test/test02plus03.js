
/* prototype取得 */
_p = test02plus03.prototype;


/* this function came from test02 */
// 謎フック設定
// これは、コマンドデザインパターンのsender側らしいようだ。
_p.send = function() {
  var sendCommand = application.getComponentById( "send-command" );
  var inputField1 = application.getComponentById( "inputField1" );
  var operandButton = application.getComponentById( "operandButton" );
  var inputField2 = application.getComponentById( "inputField2" );
  var status = application.getComponentById( "status" );
  var pb = application.getComponentById( "pb" );

  // 二重送信を防ぐ為に、一時的に入力禁止にする
  inputField1.setEnabled( false );
  operandButton.setEnabled( false );
  inputField2.setEnabled( false );
  sendCommand.setEnabled( false );

  this._startTime = new Date;
  status.setText( "Loading..." );
  pb.setVisible( true );
  pb.start();
  // find the name of the function to call
  var sFunctionName;
  switch ( operandButton.getText() ) {
    case "+":
      sFunctionName = "math.add";
    break;
    case "-":
      sFunctionName = "math.sub";
    break;
    case "*":
      sFunctionName = "math.mul";
    break;
    case "/":
      sFunctionName = "math.div";
    break;
  }
  // call the server using this name
  // ToDo: inputField1, 2が非数値だった時のエラー処理が必要
  this.xmlRpc.asyncInvoke(
      sFunctionName,
      new BiXmlRpcDouble( inputField1.getText() ),
      new BiXmlRpcDouble( inputField2.getText() ) );
};


_p.onCallComplete = function(e) {
  var sendCommand = application.getComponentById( "send-command" );
  var inputField1 = application.getComponentById( "inputField1" );
  var operandButton = application.getComponentById( "operandButton" );
  var inputField2 = application.getComponentById( "inputField2" );
  var resultField = application.getComponentById( "resultField" );
  var status = application.getComponentById( "status" );
  var pb = application.getComponentById( "pb" );

  try {
    //
    var res = this.xmlRpc.getResult();
  }
  catch ( ex ) {
    res = ex.getFaultString();
  }
  finally {
    status.setText( "Loaded in " + (new Date - this._startTime) + "ms" );

    resultField.setText( res );

    // 二重送信を防ぐ為に、一時的に入力禁止にしたものを解除する
    inputField1.setEnabled( true );
    operandButton.setEnabled( true );
    inputField2.setEnabled( true );
    sendCommand.setEnabled( true );

    pb.stop();
    pb.setVisible( false );
  }
};



function test02plus03() {
  /* create XML-RPC client */
  /* ToDo: ↓のurlをxmlの方に出し、そこから取り出すようにする。
   *         (どうやるか調べる事！)
   */
  // "http://www.tir.ne.jp/~nekoie/b/xsm.cgi";
  this.xmlRpc = new BiXmlRpc(new BiUri(application.getAdfPath(), "./xsm.cgi"));
  this.xmlRpc.addEventListener( "callcomplete", this.onCallComplete, this );

  /* Create a drop down menu for the operand button */
  var operandButton = application.getComponentById("operandButton");
  var tmp_menu  = new BiMenu;
  var operands = ["+", "-", "*", "/"];
  var tmp_menuitem;
  var action_op2 = function (e) {
    operandButton.setText( e.getTarget().getText() );
  };
  for (var i = 0; i < operands.length; i++) {
    tmp_menuitem = new BiMenuItem(operands[i]);
    tmp_menuitem.addEventListener("action", action_op2, this);
    tmp_menu.add( tmp_menuitem );
  }
  // asssign the menu as a context menu and also show the menu when clicking
  // the button
  operandButton.setContextMenu(tmp_menu);
  var action_op1 = function (e) {
    tmp_menu.setLocation(operandButton.getScreenLeft(), operandButton.getScreenTop() + operandButton.getHeight());
    tmp_menu.setVisible(true);
  };
  operandButton.addEventListener("action", action_op1, this);

  /* マニュアルを見ると、resultButtonにコマンドを割り当てるのは、
   * xml側でするよりも、ココで行った方が良さそうだ。
   * ……と思っていたが、やっぱりxml側でやった方が良さそう。
   * どうする？
   */
  /*
  var resultButton = application.getComponentById("resultButton");
  var cmd = new BiCommand;
  cmd.setId("send-command");
  cmd.setShortcut("Alt+S");
  resultButton.setCommand(cmd);
  cmd.execute = function () { ... };
  // or
  // cmd.addEventListener( "execute", this.send, this );
  */

  application.getComponentById("send-command").addEventListener( "execute", this.send, this );
};




/* main */

test02plus03.main = function() {
  new test02plus03;
};


/* vim:set ts=8 sw=2 sts=2 et: */
