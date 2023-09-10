using RandM.RMLib;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Principal;

namespace RandM.fTelnetProxy {
    public class fTelnetProxy : IDisposable {
        private int _ListenCounter = 0;
        private string _LogFilename = Path.ChangeExtension(ProcessUtils.ExecutablePath, ".log");
        private object _LogLock = new object();
        private bool _Stopping = false;
        private List<WebSocketServerThread> _WebSocketServers = new List<WebSocketServerThread>();

        #region IDisposable Support
        private bool disposedValue = false; // To detect redundant calls

        protected virtual void Dispose(bool disposing) {
            if (!disposedValue) {
                if (disposing) {
                    // dispose managed state (managed objects).
                    if (!_Stopping) {
                        Stop();
                    }

                    if (_WebSocketServers != null) {
                        foreach (var server in _WebSocketServers) {
                            if (server != null) {
                                server.Dispose();
                            }
                        }
                        _WebSocketServers = null;
                    }
                }

                // free unmanaged resources (unmanaged objects)
                // set large fields to null.

                disposedValue = true;
            }
        }

        ~fTelnetProxy() {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(false);
        }

        // This code added to correctly implement the disposable pattern.
        public void Dispose() {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(true);
            GC.SuppressFinalize(this);
        }
        #endregion

        public int ClientConnectionCount {
            get {
                return _WebSocketServers.Sum(x => x.ClientConnectionCount);
            }
        }

        public void DisplayActiveConnections() {
            foreach (var server in _WebSocketServers) {
                RMLog.Info($"- Port {server.Port} has {server.ClientConnectionCount} connections:");
                server.DisplayActiveConnections();
            }
        }

        public void DropRoot(string targetUser)
        {
            // Don't do anything if no user was given, or if root was given as the target user
            if (string.IsNullOrWhiteSpace(targetUser) || targetUser.Equals("root", StringComparison.OrdinalIgnoreCase)) {
                return;
            }

            // If we're on a Unix machine, and running as root, drop privilege to the target user
            if (OSUtils.IsUnix && (WindowsIdentity.GetCurrent().Token == IntPtr.Zero))
            {
                RMLog.Info($"Switching user from 'root' to '{targetUser}'");

                using (WindowsIdentity wiTarget = new WindowsIdentity(targetUser))
                {
                    wiTarget.Impersonate();

                    using (WindowsIdentity wiCurrent = WindowsIdentity.GetCurrent())
                    {
                        if (wiCurrent.Name != targetUser)
                        {
                            throw new ArgumentOutOfRangeException(nameof(targetUser), "Requested user account '" + targetUser + "' does not exist");
                        }
                    }
                }
            }
        }

        // TODOY Consolidate with ParseEnvironmentVariables
        private void ParseCommandLineArgs() {
            string[] Args = Environment.GetCommandLineArgs();
            if (Args.Length > 1) {
                RMLog.Info("Overriding with settings from command-line");

                for (int i = 1; i < Args.Length; i++) {
                    string Arg = Args[i].TrimStart('/').TrimStart('-');
                    switch (Arg) {
                        case "c":
                        case "cert":
                            i += 1;

                            // If file doesn't exist, and it's relative, convert to absolute
                            if (!File.Exists(Args[i]) && !Path.IsPathRooted(Args[i])) {
                                Args[i] = StringUtils.PathCombine(ProcessUtils.StartupPath, Args[i]);
                            }

                            if (File.Exists(Args[i])) {
                                Config.Default.CertificateFilename = Args[i];
                                RMLog.Info("-Cert file......" + Config.Default.CertificateFilename);
                            } else {
                                RMLog.Error("-Cert file not found: '" + Args[i] + "'");
                            }
                            break;

                        case "?":
                        case "h":
                        case "help":
                            ShowHelp();
                            return;

                        case "l":
                        case "loglevel":
                            i += 1;
                            try {
                                Config.Default.LogLevel = (LogLevel)Enum.Parse(typeof(LogLevel), Args[i]);
                                RMLog.Info("-Log level......" + Config.Default.LogLevel.ToString());
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid log level: '" + Args[i] + "'");
                            }
                            break;

                        case "p":
                        case "port":
                            i += 1;
                            try {
                                Config.Default.ListenPorts = Args[i];
                                RMLog.Info("-Listen port(s)...." + string.Join(", ", Config.Default.ListenPortsArray));
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid port(s): '" + Args[i] + "'");
                            }
                            break;

                        case "pw":
                        case "password":
                            i += 1;
                            Config.Default.CertificatePassword = Args[i];
                            RMLog.Info("-Cert password..yes (hidden)");
                            break;

                        case "r":
                        case "relay":
                            i += 1;

                            // If file doesn't exist, and it's relative, convert to absolute
                            if (!File.Exists(Args[i]) && !Path.IsPathRooted(Args[i])) {
                                Args[i] = StringUtils.PathCombine(ProcessUtils.StartupPath, Args[i]);
                            }

                            if (File.Exists(Args[i])) {
                                Config.Default.RelayFilename = Args[i];
                                RMLog.Info("-Relay file....." + Config.Default.RelayFilename);
                            } else {
                                Config.Default.RelayFilename = "";
                                RMLog.Error("-Relay file not found: '" + Args[i] + "'");
                            }
                            break;

                        case "rp":
                        case "rlogin-port":
                            i += 1;
                            try {
                                Config.Default.RLoginPort = Convert.ToInt16(Args[i]);
                                if (Config.Default.RLoginPort > 0) {
                                    // TODOX If -rp is specified before -t, then this will display the wrong hostname
                                    RMLog.Info("-RLogin target.." + Config.Default.TargetHostname + ":" + Config.Default.RLoginPort.ToString());
                                } else {
                                    RMLog.Info("-RLogin target..DISABLED");
                                }
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid port: '" + Args[i] + "'");
                            }
                            break;

                        case "t":
                        case "target":
                            i += 1;
                            string TargetHostname = Config.Default.TargetHostname;
                            int TargetPort = Config.Default.TargetPort;
                            WebUtils.ParseHostPort(Args[i], ref TargetHostname, ref TargetPort);
                            Config.Default.TargetHostname = TargetHostname;
                            Config.Default.TargetPort = TargetPort;
                            if (Config.Default.TargetPort > 0) {
                                RMLog.Info("-Telnet target.." + Config.Default.TargetHostname + ":" + Config.Default.TargetPort.ToString());
                            } else {
                                RMLog.Info("-Telnet target..DISABLED");
                            }
                            break;

                        case "u":
                        case "user":
                            i += 1;
                            Config.Default.User = Args[i];
                            break;

                        default:
                            RMLog.Error("-Unknown parameter: '" + Args[i] + "'");
                            break;
                    }
                }
            }
        }

        // TODOY Consolidate with ParseCommandLine
        private void ParseEnvironmentVariables() {
            var EnvironmentVariables = Environment.GetEnvironmentVariables().Cast<DictionaryEntry>().Where(x => x.Key.ToString().ToLower().StartsWith("ftelnet_")).ToArray();
            if (EnvironmentVariables.Length > 0) {
                RMLog.Info("Overriding with settings from environment variables");

                for (int i = 0; i < EnvironmentVariables.Length; i++) {
                    string Arg = EnvironmentVariables[i].Key.ToString().Substring(8).ToLower().Replace('_', '-'); // Substring off the leading ftelnet_ and replace _ with -
                    string Value = EnvironmentVariables[i].Value.ToString();

                    switch (Arg) {
                        case "c":
                        case "cert":
                            // If file doesn't exist, and it's relative, convert to absolute
                            if (!File.Exists(Value) && !Path.IsPathRooted(Value)) {
                                Value = StringUtils.PathCombine(ProcessUtils.StartupPath, Value);
                            }

                            if (File.Exists(Value)) {
                                Config.Default.CertificateFilename = Value;
                                RMLog.Info("-Cert file......" + Config.Default.CertificateFilename);
                            } else {
                                RMLog.Error("-Cert file not found: '" + Value + "'");
                            }
                            break;

                        case "l":
                        case "loglevel":
                            try {
                                Config.Default.LogLevel = (LogLevel)Enum.Parse(typeof(LogLevel), Value);
                                RMLog.Info("-Log level......" + Config.Default.LogLevel.ToString());
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid log level: '" + Value + "'");
                            }
                            break;

                        case "p":
                        case "port":
                            try {
                                Config.Default.ListenPorts = Value;
                                RMLog.Info("-Listen port(s)...." + string.Join(", ", Config.Default.ListenPortsArray));
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid port(s): '" + Value + "'");
                            }
                            break;

                        case "pw":
                        case "password":
                            Config.Default.CertificatePassword = Value;
                            RMLog.Info("-Cert password..yes (hidden)");
                            break;

                        case "r":
                        case "relay":
                            // If file doesn't exist, and it's relative, convert to absolute
                            if (!File.Exists(Value) && !Path.IsPathRooted(Value)) {
                                Value = StringUtils.PathCombine(ProcessUtils.StartupPath, Value);
                            }

                            if (File.Exists(Value)) {
                                Config.Default.RelayFilename = Value;
                                RMLog.Info("-Relay file....." + Config.Default.RelayFilename);
                            } else {
                                Config.Default.RelayFilename = "";
                                RMLog.Error("-Relay file not found: '" + Value + "'");
                            }
                            break;

                        case "rp":
                        case "rlogin-port":
                            try {
                                Config.Default.RLoginPort = Convert.ToInt16(Value);
                                if (Config.Default.RLoginPort > 0) {
                                    // TODOX If -rp is specified before -t, then this will display the wrong hostname
                                    RMLog.Info("-RLogin target.." + Config.Default.TargetHostname + ":" + Config.Default.RLoginPort.ToString());
                                } else {
                                    RMLog.Info("-RLogin target..DISABLED");
                                }
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid port: '" + Value + "'");
                            }
                            break;

                        case "t":
                        case "target":
                            string TargetHostname = Config.Default.TargetHostname;
                            int TargetPort = Config.Default.TargetPort;
                            WebUtils.ParseHostPort(Value, ref TargetHostname, ref TargetPort);
                            Config.Default.TargetHostname = TargetHostname;
                            Config.Default.TargetPort = TargetPort;
                            if (Config.Default.TargetPort > 0) {
                                RMLog.Info("-Telnet target.." + Config.Default.TargetHostname + ":" + Config.Default.TargetPort.ToString());
                            } else {
                                RMLog.Info("-Telnet target..DISABLED");
                            }
                            break;

                        case "u":
                        case "user":
                            i += 1;
                            Config.Default.User = Value;
                            break;

                        default:
                            RMLog.Error("-Unknown parameter: '" + Arg + "'");
                            break;
                    }
                }
            }
        }

        void RMLog_Handler(object sender, RMLogEventArgs e) {
            string Message = string.Format("[{0}] [{1}] {2}{3}",
                DateTime.Now.ToString(),
                e.Level.ToString(),
                e.Message,
                Environment.NewLine);

            lock (_LogLock) {
                FileUtils.FileAppendAllText(_LogFilename, Message);

                if ((e.Level >= Config.Default.LogLevel) && (Environment.UserInteractive || OSUtils.IsUnix)) {
                    switch (e.Level) {
                        case LogLevel.Trace: Console.ForegroundColor = ConsoleColor.DarkGray; break;
                        case LogLevel.Debug: Console.ForegroundColor = ConsoleColor.Cyan; break;
                        case LogLevel.Info: Console.ForegroundColor = ConsoleColor.Gray; break;
                        case LogLevel.Warning: Console.ForegroundColor = ConsoleColor.Yellow; break;
                        case LogLevel.Error: Console.ForegroundColor = ConsoleColor.Red; break;
                        default: Console.ForegroundColor = ConsoleColor.Gray; break;
                    }
                    Console.Write(Message);
                }
            }
        }

        private void ShowHelp() {
            if ((Environment.UserInteractive) || OSUtils.IsUnix) {
                //Console.WriteLine("345678901234567890123456789012345678901234567890123456789012345678901234567890");
                Console.WriteLine();                                                                               //
                Console.WriteLine("Usage: " + Path.GetFileName(ProcessUtils.ExecutablePath) + " [parameters]");    //
                Console.WriteLine();                                                                               //
                Console.WriteLine("Service-mode parameters:");                                                     //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /install, --install       Install the service");                              //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /uninstall, --uninstall   Uninstall the service");                            //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  Edit the " + Config.Default.FileName + " file to configure");                 //
                Console.WriteLine();                                                                               //
                Console.WriteLine();                                                                               //
                Console.WriteLine("Console-mode parameters:");                                                     //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /p <port>                  Port to listen for connections on");               //
                Console.WriteLine("  --port <port>              Default is 80");                                   //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /t <host:port>             Telnet server to redirect to");                    //
                Console.WriteLine("  --target <host:port>       Default is localhost:23");                         //
                Console.WriteLine("                             Use port 0 to disable telnet (ie localhost:0)");   //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /rp <port>                 RLogin port to redirect to");                      //
                Console.WriteLine("  --rlogn-port <port>        Default is 513");                                  //
                Console.WriteLine("  --rlogn-port <port>        Use port 0 to disable rlogin (ie -rp 0)");         //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /c <filename>              PKCS12 file containing private key + cert chain"); //
                Console.WriteLine("  --cert <filename>          Needed if your site uses https://");               //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /pw <password>             Password to open the PKCS12 file");                //
                Console.WriteLine("  --password <password>      Needed if your PKCS12 file is password protected");//
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /l <level>                 Log level (Trace, Debug, Info, Warning, Error)");  //
                Console.WriteLine("  --loglevel <level>         Default is Info");                                 //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /u <name>                  Which user to run as after binding to ports");     //
                Console.WriteLine("  --user <name>              Only applies to *nix when starting as root");      //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /?, /h, --help             Display this screen");                             //
                Console.WriteLine();                                                                               //
                //Console.WriteLine("345678901234567890123456789012345678901234567890123456789012345678901234567890");
                Environment.Exit(1);
            }
        }

        public void Start() {
            RMLog.Handler += RMLog_Handler;

            RMLog.Info("fTelnetProxy Starting Up");

            Config.Default.Load();
            ParseEnvironmentVariables();
            ParseCommandLineArgs();

            if (!string.IsNullOrWhiteSpace(Config.Default.CertificateFilename) && File.Exists(Config.Default.CertificateFilename)) {
                try {
                    if (OSUtils.IsUnix)
                    {
                        Environment.SetEnvironmentVariable("MONO_TLS_PROVIDER", "btls");
                    }

                    // Try loading the certificate to ensure there are no problems
                    var testCert = Config.Default.Certificate;
                } catch (Exception ex) {
                    // Loading cert failed, so wipe the filename to ensure we don't try loading it again later
                    RMLog.Exception(ex, "--Error loading cert file");
                    Config.Default.CertificateFilename = null;
                }
            }

            foreach (int port in Config.Default.ListenPortsArray) {
                try {
                    RMLog.Info($"Starting WebSocket proxy thread for port {port}");

                    var server = new WebSocketServerThread("0.0.0.0", port);
                    server.ClientCountEvent += WebSocketServer_ClientCountEvent;
                    server.ListeningEvent += WebSocketServer_ListeningEvent;
                    server.Start();

                    _WebSocketServers.Add(server);
                } catch (Exception ex) {
                    RMLog.Exception(ex, $"Failed to start WebSocket proxy thread for port {port}");
                    Environment.Exit(1);
                }
            }
        }

        public void Stop() {
            _Stopping = true;

            RMLog.Info("fTelnetProxy shutting down");

            if (_WebSocketServers != null) {
                while (_WebSocketServers.Any()) {
                    var server = _WebSocketServers.First();
                    if (server != null) {
                        RMLog.Info($"Stopping WebSocket proxy thread for port {server.Port}");
                        server.Stop();
                        server.WaitFor();
                    }

                    _WebSocketServers.Remove(server);
                }
            }

            RMLog.Info("fTelnetProxy terminated");

            FileUtils.FileAppendAllText(_LogFilename, Environment.NewLine + Environment.NewLine);
        }

        private void WebSocketServer_ClientCountEvent(object sender, EventArgs e) {
            RMLog.Trace(ClientConnectionCount + " active connections");
        }

        private void WebSocketServer_ListeningEvent(object sender, EventArgs e)
        {
            // Drop root once all server threads are listening
            _ListenCounter += 1;
            if (_ListenCounter == Config.Default.ListenPortsArray.Length) {
                DropRoot(Config.Default.User);
            }
        }
    }
}
