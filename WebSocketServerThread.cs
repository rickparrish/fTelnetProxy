using System.Collections.Generic;
using System;
using System.Net.Sockets;

namespace RandM.RMLib
{
    public class WebSocketServerThread : RMThread
    {
        public event EventHandler<ConnectionAcceptedEventArgs> ConnectionAcceptedEvent = null;
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private object _Lock = new object();

        private List<WebSocketClientThread> _Connections = new List<WebSocketClientThread>();
        private TcpConnection _Server = null;

        private string _Address;
        private int _Port;

        public WebSocketServerThread(string address, int port)
        {
            _Address = address;
            _Port = port;

            _Server = new WebSocketConnection();
            _Stop = !_Server.Listen(_Address, _Port);
            if (_Stop)
            {
                throw new ApplicationException("Unable to listen on " + _Address + ":" + _Port.ToString());
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
                        Socket NewSocket = _Server.Accept();
                        if (NewSocket != null)
                        {
                            WebSocketConnection NewConnection = new WebSocketConnection(true);
                            if (NewConnection.Open(NewSocket))
                            {
                                ConnectionAcceptedEventArgs.Raise(this, ConnectionAcceptedEvent, _Address, _Port, NewConnection.GetRemoteIP(), NewConnection.GetRemotePort());
                                RaiseMessageEvent("Connection accepted from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());

                                WebSocketClientThread NewClient = new WebSocketClientThread(NewConnection);
                                NewClient.CloseEvent += new System.EventHandler(NewClient_CloseEvent);
                                NewClient.ErrorMessageEvent += new EventHandler<StringEventArgs>(ProxyClient_ErrorMessageEvent);
                                NewClient.MessageEvent += new EventHandler<StringEventArgs>(ProxyClient_MessageEvent);
                                lock (_Lock)
                                {
                                    _Connections.Add(NewClient);
                                }
                                NewClient.Start();
                            }
                            else
                            {
                                RaiseErrorMessageEvent("Invalid WebSocket connection from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                            }
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
                _Connections.Remove((WebSocketClientThread)sender);
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
            if (Handler != null) Handler(this, new StringEventArgs("[" + _Address + ":" + _Port.ToString() + "] " + AMessage));
        }

        private void RaiseMessageEvent(string AMessage)
        {
            EventHandler<StringEventArgs> Handler = MessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs("[" + _Address + ":" + _Port.ToString() + "] " + AMessage));
        }

        public override void Stop()
        {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            if (_Server != null) _Server.Close();

            base.Stop();
        }
    }
}
