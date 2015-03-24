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
        
        FlashSocketPolicyServerThread _FSPS = null;
        ProxyServerThread _WebSocketProxy = null;

        public fTelnetProxy()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Starting Up"));

            MessageEvent(null, new StringEventArgs("Starting Flash Socket Policy Thread"));
            try
            {
                _FSPS = new FlashSocketPolicyServerThread("0.0.0.0", 843, "1123"); // TODO 0.0.0.0, 843, 1123
                _FSPS.ConnectionAcceptedEvent += FSPS_ConnectionAcceptedEvent;
                _FSPS.ErrorMessageEvent += ErrorMessageEvent;
                _FSPS.MessageEvent += MessageEvent;
                _FSPS.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start Flash Socket Policy Server Thread: " + ex.Message));
                _FSPS = null;
            }

            MessageEvent(null, new StringEventArgs("Starting WebSocket Proxy Thread"));
            try
            {
                _WebSocketProxy = new ProxyServerThread("0.0.0.0", 1123, ConnectionType.WebSocket, ConnectionType.None); // TODO 0.0.0.0, 1123
                _WebSocketProxy.ConnectionAcceptedEvent += WebSocketProxy_ConnectionAcceptedEvent;
                _WebSocketProxy.ErrorMessageEvent += ErrorMessageEvent;
                _WebSocketProxy.MessageEvent += MessageEvent;
                _WebSocketProxy.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start WebSocket Proxy Thread: " + ex.Message));
                _WebSocketProxy = null;
            }
        }

        public void Dispose()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Shutting Down"));

            if (_WebSocketProxy != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping WebSocket Proxy Thread"));
                _WebSocketProxy.Stop();
            }
            if (_FSPS != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping Flash Socket Policy Thread"));
                _FSPS.Stop();
            }

            MessageEvent(null, new StringEventArgs("fTelnetProxy Terminated" + Environment.NewLine + Environment.NewLine));
        }

        private void ErrorMessageEvent(object sender, StringEventArgs mea)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] " + mea.Text + Environment.NewLine;
            FileUtils.FileAppendAllText(_ErrorLogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }

        void FSPS_ConnectionAcceptedEvent(object sender, ConnectionAcceptedEventArgs e)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] Accepted FSPS connection from " + e.RemoteIP + ":" + e.RemotePort.ToString();
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
