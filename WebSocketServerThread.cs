// TODO Check for code that needs to be rewritten
using RandM.RMLib;
using System;
using System.IO;
using System.Net.Sockets;
using System.Security.Cryptography.X509Certificates;
using System.Text;

namespace RandM.fTelnetProxy
{
    public class WebSocketServerThread : RMThread
    {
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
                        try
                        {
                            // Accept an incoming connection
                            if (_Server.CanAccept(500)) // 1/2 of a second
                            {
                                Socket NewSocket = _Server.Accept();
                                if (NewSocket != null)
                                {
                                    // TODO Need to pass in accepted protocols and retrieve requested server and ignore /ping
                                    WebSocketConnection NewConnection = new WebSocketConnection(true);
                                    if (Config.Default.CertFilename != "")
                                    {
                                        try
                                        {
                                            NewConnection.Certificate = new X509Certificate2(Config.Default.CertFilename, Config.Default.CertPassword);
                                        }
                                        catch (Exception ex)
                                        {
                                            RMLog.Exception(ex, "Unable to load PFX file '" + Config.Default.CertFilename + "'");
                                        }
                                    }
                                    if (NewConnection.Open(NewSocket))
                                    {
                                        if (NewConnection.Header["Path"] == "/ping")
                                        {
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
                                            RMLog.Info("Connection accepted from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());

                                            string MessageText = string.Format("{0}\t{1}\t{2}\t{3}\t{4}\r\n", "TODO scheme", NewConnection.GetRemoteIP(), NewConnection.GetRemotePort(), "TODO clientConnection.ConnectionInfo.Path", "TODO clientConnection.ConnectionInfo.NegotiatedSubProtocol");
                                            byte[] MessageBytes = Encoding.ASCII.GetBytes(MessageText);
                                            LogStream.Write(MessageBytes, 0, MessageBytes.Length);
                                            LogStream.Flush();

                                            WebSocketClientThread NewClient = new WebSocketClientThread(NewConnection);
                                            NewClient.Start();
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
