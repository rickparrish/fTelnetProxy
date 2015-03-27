using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace RandM.fTelnetProxy
{
    public class WebSocketServerThread : RMThread
    {
        private string _Address;
        private List<WebSocketClientThread> ClientThreads = new List<WebSocketClientThread>();
        private object ClientThreadsLock = new object();
        private int _Port;
        private TcpConnection _Server = null;

        public WebSocketServerThread(string address, int port)
        {
            _Address = address;
            _Port = port;
        }

        void ClientThread_FinishEvent(object sender, EventArgs e)
        {
            lock (ClientThreadsLock)
            {
                if ((sender is WebSocketClientThread) && ClientThreads.Contains((WebSocketClientThread)sender))
                {
                    ClientThreads.Remove((WebSocketClientThread)sender);
                }
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

                                            string MessageText = string.Format("{0}\t{1}\t{2}\t{3}\t{4}\t{5}\r\n", 
                                                DateTime.Now.ToString(),
                                                NewConnection.GetRemoteIP(), 
                                                NewConnection.GetRemotePort(),
                                                NewConnection.Header["Path"],
                                                NewConnection.Protocol,
                                                NewConnection.SubProtocol);
                                            byte[] MessageBytes = Encoding.ASCII.GetBytes(MessageText);
                                            LogStream.Write(MessageBytes, 0, MessageBytes.Length);
                                            LogStream.Flush();

                                            WebSocketClientThread ClientThread = new WebSocketClientThread(NewConnection);
                                            ClientThread.FinishEvent += ClientThread_FinishEvent;
                                            lock (ClientThreadsLock)
                                            {
                                                ClientThreads.Add(ClientThread);
                                            }
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
                    int ClientThreadCount = 0;
                    lock (ClientThreadsLock)
                    {
                        foreach (var ClientThread in ClientThreads)
                        {
                            if (ClientThread != null) ClientThread.Stop();
                        }
                        ClientThreadCount = ClientThreads.Count;
                    }

                    // Wait for client threads
                    while (ClientThreadCount > 0)
                    {
                        lock (ClientThreadsLock)
                        {
                            ClientThreadCount = ClientThreads.Count;
                        }
                        Thread.Sleep(100);
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
