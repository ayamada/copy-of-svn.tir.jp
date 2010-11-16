function test03()
{
	var win = application.getWindow();

	// create XML-RPC client
	this.xmlRpc = new BiXmlRpc( test03.XML_RPC_URI );
	this.xmlRpc.addEventListener( "callcomplete", this.onCallComplete, this );

	// create the GUI components
	this.inputField1 = new BiTextField("10");
	this.inputField2 = new BiTextField("3");
	this.resultField = new BiTextField;
	this.operandButton = new BiButton("/");
	this.resultButton = new BiButton("=");
	this.statusBar = new BiStatusBar;
	this.statusSection = new BiStatusBarPanel( "Done" );

	this.inputField1.setAlign("right");
	this.inputField2.setAlign("right");
	this.resultField.setAlign("right");
	this.resultField.setReadOnly(true);

	this.inputField1.setTabIndex(1);
	this.operandButton.setTabIndex(2);
	this.inputField2.setTabIndex(3);
	this.resultButton.setTabIndex(4);
	this.resultField.setTabIndex(5);

	win.add(this.inputField1);
	win.add(this.operandButton);
	win.add(this.inputField2);
	win.add(this.resultButton);
	win.add(this.resultField);
	this.statusBar.add(this.statusSection);
	win.add(this.statusBar);

	this.statusSection.setLeft(0);
	this.statusSection.setRight(0);
	this.statusSection.setPadding(2, 1);

	// make pressing Enter dispatch the action for the result button
	win.setAcceptButton(this.resultButton);

	// do the layout
	this.inputField1.setLocation(5, 7);
	this.inputField1.setWidth(50);
	this.operandButton.setLocation(this.inputField1.getLeft() + this.inputField1.getWidth() + 5, 5);
	this.operandButton.setWidth(20);
	this.inputField2.setLocation(this.operandButton.getLeft() + this.operandButton.getWidth() + 5, 7);
	this.inputField2.setWidth(50);
	this.resultButton.setLocation(this.inputField2.getLeft() + this.inputField2.getWidth() + 5, 5);
	this.resultButton.setWidth(20);
	this.resultField.setLocation(this.resultButton.getLeft() + this.resultButton.getWidth() + 5, 7);
	this.resultField.setRight(5);

	this.resultButton.addEventListener("action", this.getResult, this);

	// Create a drop down menu for the operand button
	var m  = new BiMenu;
	var operands = ["+", "-", "*", "/"];
	var tmp;
	for (var i = 0; i < operands.length; i++) {
		m.add( tmp = new BiMenuItem(operands[i]) );
		tmp.addEventListener("action", function (e) {
			this.operandButton.setText( e.getTarget().getText() );
		}, this);
	}

	// asssign the menu as a context menu and also show the menu when clicking
	// the button
	this.operandButton.setContextMenu(m);
	this.operandButton.addEventListener("action", function (e) {
		m.setLocation(this.operandButton.getScreenLeft(), this.operandButton.getScreenTop() + this.operandButton.getHeight());
		m.setVisible(true);
	}, this);
}

test03.main = function () { new test03; };

/*
test03.XML_RPC_URI = "../../services/xmlrpctest.aspx";
test03.XML_RPC_URI = new BiUri( "http://www.bindows.net/services/xmlrpctest.aspx" );
*/
test03.XML_RPC_URI = new BiUri( "http://www.tir.ne.jp/~nekoie/b/xsm.cgi" );


test03.prototype.onCallComplete = function ( e )
{
	if ( !e.getError() )
	{
		var res = e.getResult();
		this.resultField.setText( res );
		this.statusSection.setText( "Done" );
	}
	else
	{
		var eo = e.getErrorObject();
		this.statusSection.setText( "Done but with errors" );
		alert( "Got an error\n\n" + eo.getFaultString() );
	}
};

test03.prototype.getResult = function ()
{
	// find the name of the function to call
	var sFunctionName;
	switch ( this.operandButton.getText() )
	{
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

	this.statusSection.setText( "Calling " + sFunctionName + "..." );
	// call the server using this name
	this.xmlRpc.asyncInvoke( sFunctionName,
			new BiXmlRpcDouble( this.inputField1.getText() ),
			new BiXmlRpcDouble( this.inputField2.getText() ) );
};
/* vim:set ts=8 sw=8 sts=8: */
