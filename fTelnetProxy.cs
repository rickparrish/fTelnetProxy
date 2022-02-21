using RandM.RMLib;
using System;
using System.Collections;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;

namespace RandM.fTelnetProxy {
    public class fTelnetProxy : IDisposable {
        private string _LogFilename = Path.ChangeExtension(ProcessUtils.ExecutablePath, ".log");
        private object _LogLock = new object();
        private bool _Stopping = false;
        private WebSocketServerThread _WebSocketServer = null;

        #region IDisposable Support
        private bool disposedValue = false; // To detect redundant calls

        protected virtual void Dispose(bool disposing) {
            if (!disposedValue) {
                if (disposing) {
                    // dispose managed state (managed objects).
                    if (!_Stopping) Stop();
                    if (_WebSocketServer != null) _WebSocketServer.Dispose();
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
                return _WebSocketServer.ClientConnectionCount;
            }
        }

        public void DisplayActiveConnections() {
            RMLog.Info($" - {_WebSocketServer.ClientConnectionCount} connections:");
            _WebSocketServer.DisplayActiveConnections();
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
                                RMLog.Level = (LogLevel)Enum.Parse(typeof(LogLevel), Args[i]);
                                RMLog.Info("-Log level......" + RMLog.Level.ToString());
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid log level: '" + Args[i] + "'");
                            }
                            break;

                        case "p":
                        case "port":
                            i += 1;
                            try {
                                Config.Default.ListenPort = Convert.ToInt16(Args[i]);
                                RMLog.Info("-Listen port...." + Config.Default.ListenPort.ToString());
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid port: '" + Args[i] + "'");
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
                                RMLog.Level = (LogLevel)Enum.Parse(typeof(LogLevel), Value);
                                RMLog.Info("-Log level......" + RMLog.Level.ToString());
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid log level: '" + Value + "'");
                            }
                            break;

                        case "p":
                        case "port":
                            try {
                                Config.Default.ListenPort = Convert.ToInt16(Value);
                                RMLog.Info("-Listen port...." + Config.Default.ListenPort.ToString());
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "-Invalid port: '" + Value + "'");
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

                if ((Environment.UserInteractive) || OSUtils.IsUnix) {
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
                Console.WriteLine("  /i, -i, /install, --install       Install the service");                      //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  /u, -u, /uninstall, --uninstall   Uninstall the service");                    //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  Edit the " + Config.Default.FileName + " file to configure");                 //
                Console.WriteLine();                                                                               //
                Console.WriteLine();                                                                               //
                Console.WriteLine("Console-mode parameters:");                                                     //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -p <port>                  Port to listen for connections on");               //
                Console.WriteLine("  --port <port>              Default is 80");                                   //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -t <host:port>             Telnet server to redirect to");                    //
                Console.WriteLine("  --target <host:port>       Default is localhost:23");                         //
                Console.WriteLine("                             Use port 0 to disable telnet (ie localhost:0)");   //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -rp <port>                 RLogin port to redirect to");                      //
                Console.WriteLine("  --rlogn-port <port>        Default is 513");                                  //
                Console.WriteLine("  --rlogn-port <port>        Use port 0 to disable rlogin (ie -rp 0)");         //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -c <filename>              PKCS12 file containing private key + cert chain"); //
                Console.WriteLine("  --cert <filename>          Needed if your site uses https://");               //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -pw <password>             Password to open the PKCS12 file");                //
                Console.WriteLine("  --password <password>      Needed if your PKCS12 file is password protected");//
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -l <level>                 Log level (Trace, Debug, Info, Warning, Error)");  //
                Console.WriteLine("  --loglevel <level>         Default is Info");                                 //
                Console.WriteLine();                                                                               //
                Console.WriteLine("  -?, -h, --help             Display this screen");                             //
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

            if ((Config.Default.CertificateFilename != "") && File.Exists(Config.Default.CertificateFilename)) {
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

            try {
                RMLog.Info("Starting WebSocket proxy thread");
                _WebSocketServer = new WebSocketServerThread("0.0.0.0", Config.Default.ListenPort);
                _WebSocketServer.Start();
            } catch (Exception ex) {
                RMLog.Exception(ex, "Failed to start WebSocket proxy thread");
                Environment.Exit(1);
            }
        }

        public void Stop() {
            _Stopping = true;

            RMLog.Info("fTelnetProxy shutting down");

            if (_WebSocketServer != null) {
                RMLog.Info("Stopping WebSocket proxy thread");
                _WebSocketServer.Stop();
                _WebSocketServer.WaitFor();
            }

            RMLog.Info("fTelnetProxy terminated");

            FileUtils.FileAppendAllText(_LogFilename, Environment.NewLine + Environment.NewLine);
        }
    }
}
