﻿using System.Collections.Generic;
using System;
using System.Net.Sockets;
using System.IO;
using System.Text;

namespace RandM.RMLib
{
    public class WebSocketServerThread : RMThread
    {
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private string _Address;
        private int _Port;
        private TcpConnection _Server = null;

        public WebSocketServerThread(string address, int port)
        {
            _Address = address;
            _Port = port;
        }

        protected override void Execute()
        {
            _Server = new WebSocketConnection();
            if (_Server.Listen(_Address, _Port))
            {
                using (FileStream LogStream = new FileStream(Path.Combine(ProcessUtils.StartupPath, "fTelnetProxy-Connections.log"), FileMode.Append, FileAccess.Write, FileShare.Read))
                {
                    while (!_Stop)
                    {
                        // Accept an incoming connection
                        if (_Server.CanAccept(500)) // 1/2 of a second
                        {
                            Socket NewSocket = _Server.Accept();
                            if (NewSocket != null)
                            {
                                // TODO Need to pass in accepted protocols and retrieve requested server and ignore /ping
                                WebSocketConnection NewConnection = new WebSocketConnection(true);
                                if (NewConnection.Open(NewSocket))
                                {
                                    RaiseMessageEvent("Connection accepted from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());
                                    
                                    string MessageText = string.Format("{0}\t{1}\t{2}\t{3}\t{4}\r\n", "TODO scheme", NewConnection.GetRemoteIP(), NewConnection.GetRemotePort(), "TODO clientConnection.ConnectionInfo.Path", "TODO clientConnection.ConnectionInfo.NegotiatedSubProtocol");
                                    byte[] MessageBytes = Encoding.ASCII.GetBytes(MessageText);
                                    LogStream.Write(MessageBytes, 0, MessageBytes.Length);
                                    LogStream.Flush();

                                    WebSocketClientThread NewClient = new WebSocketClientThread(NewConnection);
                                    NewClient.ErrorMessageEvent += new EventHandler<StringEventArgs>(ProxyClient_ErrorMessageEvent);
                                    NewClient.MessageEvent += new EventHandler<StringEventArgs>(ProxyClient_MessageEvent);
                                    NewClient.Start();
                                }
                                else
                                {
                                    RaiseErrorMessageEvent("Invalid WebSocket connection from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                RaiseErrorMessageEvent("WebSocket Server Thread: Unable to listen on " + _Address + ":" + _Port);
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