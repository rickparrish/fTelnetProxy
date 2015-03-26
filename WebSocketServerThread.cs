using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Sockets;
using System.Security.Cryptography.X509Certificates;
using System.Text;

namespace RandM.fTelnetProxy
{
    public class WebSocketServerThread : RMThread
    {
        private string _Address;
        private List<WebSocketClientThread> ClientThreads = new List<WebSocketClientThread>();
        private int _Port;
        private TcpConnection _Server = null;

        public WebSocketServerThread(string address, int port)
        {
            _Address = address;
            _Port = port;
        }

        void ClientThread_FinishEvent(object sender, EventArgs e)
        {
            if ((sender is WebSocketClientThread) && ClientThreads.Contains((WebSocketClientThread)sender))
            {
                ClientThreads.Remove((WebSocketClientThread)sender);
            }
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
                        try
                        {
                            // Accept an incoming connection
                            if (_Server.CanAccept(500)) // 1/2 of a second
                            {
                                Socket NewSocket = _Server.Accept();
                                if (NewSocket != null)
                                {
                                    WebSocketConnection NewConnection = new WebSocketConnection(true, Config.Default.Certificate);
                                    if (NewConnection.Open(NewSocket))
                                    {
                                        if (NewConnection.Header["Path"] == "/ping")
                                        {
                                            // Handle ping requests (from proxy.ftelnet.ca most likely)
                                            string Ping = NewConnection.ReadLn(1000);
                                            if (NewConnection.ReadTimedOut)
                                            {
                                                RMLog.Debug("Answering a /ping (no time received) from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());
                                            }
                                            else
                                            {
                                                RMLog.Debug("Answering a /ping (" + Ping + ") from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());
                                                NewConnection.Write(Ping);
                                            }
                                            NewConnection.Close();
                                        }
                                        else
                                        {
                                            // Handle normal connection
                                            RMLog.Info("Connection accepted from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());

                                            string MessageText = string.Format("{0}\t{1}\t{2}\t{3}\t{4}\r\n", "TODO scheme", NewConnection.GetRemoteIP(), NewConnection.GetRemotePort(), "TODO clientConnection.ConnectionInfo.Path", "TODO clientConnection.ConnectionInfo.NegotiatedSubProtocol");
                                            byte[] MessageBytes = Encoding.ASCII.GetBytes(MessageText);
                                            LogStream.Write(MessageBytes, 0, MessageBytes.Length);
                                            LogStream.Flush();

                                            WebSocketClientThread ClientThread = new WebSocketClientThread(NewConnection);
                                            ClientThread.FinishEvent += ClientThread_FinishEvent;
                                            ClientThreads.Add(ClientThread);
                                            ClientThread.Start();
                                        }
                                    }
                                    else
                                    {
                                        if (NewConnection.FlashPolicyFileRequest)
                                        {
                                            RMLog.Info("Answered flash policy file request from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                                        }
                                        else
                                        {
                                            RMLog.Warning("Invalid WebSocket connection from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                                        }
                                        NewConnection.Close();
                                    }
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            RMLog.Exception(ex, "Unable to accept new websocket connection");
                        }
                    }

                    // Stop client threads
                    foreach (var ClientThread in ClientThreads)
                    {
                        if (ClientThread != null) ClientThread.Stop();
                    }

                    // Wait for client threads
                    foreach (var ClientThread in ClientThreads)
                    {
                        if (ClientThread != null) ClientThread.WaitFor();
                    }
                }
            }
            else
            {
                RMLog.Error("WebSocket Server Thread: Unable to listen on " + _Address + ":" + _Port);
            }
        }

        public override void Stop()
        {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            if (_Server != null) _Server.Close();

            base.Stop();
        }
    }
}
