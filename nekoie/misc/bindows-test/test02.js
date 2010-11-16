
function test02()
{
	var sendCommand = application.getComponentById( "send-command" );
	var jsSource = application.getComponentById( "js-source" );
	var result= application.getComponentById( "result" );

	sendCommand.addEventListener( "execute", this.send, this );

	this._xmlRpc = new BiXmlRpc;
	this._xmlRpc.useService( test02.XML_RPC_URI );
	this._xmlRpc.addEventListener( "callcomplete", this.onCallComplete, this );
}

/*
test02.XML_RPC_URI = "../../services/xmlrpctest.aspx";
test02.XML_RPC_URI = new BiUri( "http://www.bindows.net/services/xmlrpctest.aspx" );
*/
test02.XML_RPC_URI = new BiUri( "http://www.tir.ne.jp/~nekoie/b/xsm.cgi" );

_p = test02.prototype;

_p.send = function ()
{
	var sendCommand = application.getComponentById( "send-command" );
	var jsSource = application.getComponentById( "js-source" );
	var result = application.getComponentById( "result" );
	var status = application.getComponentById( "status" );
	var pb = application.getComponentById( "pb" );

	var data = eval( "data = " + jsSource.getText() );

	sendCommand.setEnabled( false );
	jsSource.setEnabled( false );

	this._startTime = new Date;
	status.setText( "Loading..." );
	pb.setVisible( true );
	pb.start();
	this._xmlRpc.callMethod( "echo", data );
};

_p.onCallComplete = function ( e )
{
	var sendCommand = application.getComponentById( "send-command" );
	var jsSource = application.getComponentById( "js-source" );
	var result= application.getComponentById( "result" );
	var status = application.getComponentById( "status" );
	var pb = application.getComponentById( "pb" );

	try {
		var res = this._xmlRpc.getResult();
	}
	catch ( ex )
	{
		res = ex.getFaultString();
	}
	finally
	{
		status.setText( "Loaded in " + (new Date - this._startTime) + "ms" );

		result.setText( res );

		sendCommand.setEnabled( true );
		jsSource.setEnabled( true );

		pb.stop();
		pb.setVisible( false );
	}
};

test02.main = function ()
{
	new test02;
};


