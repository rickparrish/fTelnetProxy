using RandM.RMLib;
using System;
using System.IO;
using System.Reflection;

namespace RandM.fTelnetProxy
{
    public class fTelnetProxy : IDisposable
    {
        private string _ConnectionLogFile = StringUtils.PathCombine(ProcessUtils.StartupPath, "Connection.log");
        private string _ErrorLogFile = StringUtils.PathCombine(ProcessUtils.StartupPath, "Error.log");
        private string _LogFile = StringUtils.PathCombine(ProcessUtils.StartupPath, "fTelnetProxy.log");
        
        FlashSocketPolicyServerThread _FlashSocketPolicyServer = null;
        WebSocketServerThread _WebSocketServer = null;

        public fTelnetProxy()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Starting Up"));

            MessageEvent(null, new StringEventArgs("Starting Flash Socket Policy Thread"));
            try
            {
                _FlashSocketPolicyServer = new FlashSocketPolicyServerThread("0.0.0.0", 843, "1123"); // TODO 0.0.0.0, 843, 1123
                _FlashSocketPolicyServer.ConnectionAcceptedEvent += FlashSocketPolicyServer_ConnectionAcceptedEvent;
                _FlashSocketPolicyServer.ErrorMessageEvent += ErrorMessageEvent;
                _FlashSocketPolicyServer.MessageEvent += MessageEvent;
                _FlashSocketPolicyServer.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start Flash Socket Policy Server Thread: " + ex.Message));
                _FlashSocketPolicyServer = null;
            }

            MessageEvent(null, new StringEventArgs("Starting WebSocket Proxy Thread"));
            try
            {
                _WebSocketServer = new WebSocketServerThread("0.0.0.0", 1123); // TODO 0.0.0.0, 1123
                _WebSocketServer.ConnectionAcceptedEvent += WebSocketProxy_ConnectionAcceptedEvent;
                _WebSocketServer.ErrorMessageEvent += ErrorMessageEvent;
                _WebSocketServer.MessageEvent += MessageEvent;
                _WebSocketServer.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start WebSocket Proxy Thread: " + ex.Message));
                _WebSocketServer = null;
            }
        }

        public void Dispose()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Shutting Down"));

            if (_WebSocketServer != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping WebSocket Proxy Thread"));
                _WebSocketServer.Stop();
            }
            if (_FlashSocketPolicyServer != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping Flash Socket Policy Thread"));
                _FlashSocketPolicyServer.Stop();
            }

            MessageEvent(null, new StringEventArgs("fTelnetProxy Terminated" + Environment.NewLine + Environment.NewLine));
        }

        private void ErrorMessageEvent(object sender, StringEventArgs mea)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] " + mea.Text + Environment.NewLine;
            FileUtils.FileAppendAllText(_ErrorLogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }

        void FlashSocketPolicyServer_ConnectionAcceptedEvent(object sender, ConnectionAcceptedEventArgs e)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] Accepted Flash Socket Policy Server connection from " + e.RemoteIP + ":" + e.RemotePort.ToString();
            FileUtils.FileAppendAllText(_ConnectionLogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }

        private void MessageEvent(object sender, StringEventArgs mea)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] " + mea.Text + Environment.NewLine;
            FileUtils.FileAppendAllText(_LogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }

        void WebSocketProxy_ConnectionAcceptedEvent(object sender, ConnectionAcceptedEventArgs e)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] Accepted WebSocket connection from " + e.RemoteIP + ":" + e.RemotePort.ToString();
            FileUtils.FileAppendAllText(_ConnectionLogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }
    }
}
