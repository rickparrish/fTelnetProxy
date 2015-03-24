using System.Collections.Generic;
using System;

namespace RandM.RMLib
{
    public class ProxyServerThread : RMThread
    {
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private object _Lock = new object();

        private List<ProxyClientThread> _Connections = new List<ProxyClientThread>();
        private TcpConnection _Server = null;

        private string _InIP;
        private int _InPort;
        private ConnectionType _InConnectionType;
        private ConnectionType _OutConnectionType;

        public ProxyServerThread(string inIP, int inPort, ConnectionType inConnectionType, ConnectionType outConnectionType)
        {
            _InIP = inIP;
            _InPort = inPort;
            _InConnectionType = inConnectionType;
            _OutConnectionType = outConnectionType;

            switch (_InConnectionType)
            {
                case ConnectionType.None: _Server = new TcpConnection(); break;
                case ConnectionType.RLogin: _Server = new RLoginConnection(); break;
                case ConnectionType.Telnet: _Server = new TelnetConnection(); break;
                case ConnectionType.WebSocket: _Server = new WebSocketConnection(); break;
                default: throw new ArgumentException("ProxyServer unknown InConnectionType: " + _InConnectionType.ToString());
            }

            _Stop = !_Server.Listen(_InIP, _InPort);
            if (_Stop)
            {
                throw new ApplicationException("Unable to listen on " + _InIP + ":" + _InPort.ToString());
            }
        }

        protected override void Execute()
        {
            if (!_Stop)
            {
                while (!_Stop)
                {
                    // Accept an incoming connection
                    if (_Server.CanAccept(500)) // 1/2 of a second
                    {
                        TcpConnection NewConnection = _Server.AcceptTCP();
                        if (NewConnection != null)
                        {
                            RaiseMessageEvent("Connection accepted from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());

                            ProxyClientThread NewClient = new ProxyClientThread(NewConnection, _OutConnectionType);
                            NewClient.CloseEvent += new System.EventHandler(NewClient_CloseEvent);
                            NewClient.ErrorMessageEvent += new EventHandler<StringEventArgs>(ProxyClient_ErrorMessageEvent);
                            NewClient.MessageEvent += new EventHandler<StringEventArgs>(ProxyClient_MessageEvent);
                            lock (_Lock)
                            {
                                _Connections.Add(NewClient);
                            }
                            NewClient.Start();
                        }
                    }
                }

                while (_Connections.Count > 0)
                {
                    _Connections[0].Stop();
                }
            }
        }

        void NewClient_CloseEvent(object sender, System.EventArgs e)
        {
            lock (_Lock)
            {
                _Connections.Remove((ProxyClientThread)sender);
            }
        }

        void ProxyClient_ErrorMessageEvent(object sender, StringEventArgs mea)
        {
            RaiseErrorMessageEvent(mea.Text);
        }

        void ProxyClient_MessageEvent(object sender, StringEventArgs mea)
        {
            RaiseMessageEvent(mea.Text);
        }

        private void RaiseErrorMessageEvent(string AMessage)
        {
            EventHandler<StringEventArgs> Handler = ErrorMessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs("[" + _InIP + ":" + _InPort.ToString() + "] " + AMessage));
        }

        private void RaiseMessageEvent(string AMessage)
        {
            EventHandler<StringEventArgs> Handler = MessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs("[" + _InIP + ":" + _InPort.ToString() + "] " + AMessage));
        }

        public override void Stop()
        {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            if (_Server != null) _Server.Close();

            base.Stop();
        }
    }
}
