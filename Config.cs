using RandM.RMLib;
using System;
using System.IO;
using System.Security.Cryptography.X509Certificates;

namespace RandM.fTelnetProxy {
    public class Config : ConfigHelper {
        public string CertificateFilename { get; set; }
        public string CertificatePassword { get; set; }
        public int ListenPort { get; set; }
        public LogLevel LogLevel { get; set; }
        public double MaxIdleTimeInMinutes { get; set; }
        public double MaxSessionLengthInHours;
        public string RelayFilename { get; set; }
        public string RelayDeniedFilename { get; set; }
        public int RLoginPort { get; set; }
        public string TargetHostname { get; set; }
        public int TargetPort { get; set; }
        public string User { get; set; }

        static public Config Default = new Config();

        public Config()
            : base(ConfigSaveLocation.Relative) {
            CertificateFilename = "";
            CertificatePassword = "";
            ListenPort = 80; // TODOX ip6tables doesn't support port forwarding, so might need option to listen on multiple ports
            LogLevel = LogLevel.Info;
            MaxIdleTimeInMinutes = 10;
            MaxSessionLengthInHours = 6;
            RelayDeniedFilename = "relay-denied.ans";
            RelayFilename = "";
            RLoginPort = 513;
            TargetHostname = "localhost";
            TargetPort = 23;
            User = null;
        }

        public X509Certificate2 Certificate { get
            {
                if (string.IsNullOrWhiteSpace(CertificateFilename) || !File.Exists(CertificateFilename)) {
                    return null;
                } else {
                    return new X509Certificate2(CertificateFilename, CertificatePassword);
                }
            }
        }

        public new void Load() {
            // Try to load, and save a new file if load failed
            if (!base.Load()) base.Save();

            RMLog.Level = LogLevel;

            // Output the settings being used
            RMLog.Info("Using settings from " + base.FileName);
            RMLog.Info("-Listen port: " + ListenPort.ToString());
            if (TargetPort > 0) {
                RMLog.Info("-Telnet target: " + TargetHostname + ":" + TargetPort.ToString());
            } else {
                RMLog.Info("-Telnet target: DISABLED");
            }
            if (RLoginPort > 0) {
                RMLog.Info("-RLogin target: " + TargetHostname + ":" + RLoginPort.ToString());
            } else {
                RMLog.Info("-RLogin target: DISABLED");
            }
            RMLog.Info("-Log level: " + LogLevel.ToString());
            if (!string.IsNullOrWhiteSpace(CertificateFilename)) {
                // If file doesn't exist, and it's relative, convert to absolute
                if (!File.Exists(CertificateFilename) && !Path.IsPathRooted(CertificateFilename)) {
                    CertificateFilename = StringUtils.PathCombine(ProcessUtils.StartupPath, CertificateFilename);
                }

                if (File.Exists(CertificateFilename)) {
                    RMLog.Info("-Cert file: " + CertificateFilename);
                    if (string.IsNullOrWhiteSpace(CertificatePassword)) {
                        RMLog.Info("-Cert password: none");
                    } else {
                        RMLog.Info("-Cert password: yes (hidden)");
                    }
                } else {
                    RMLog.Error("-Cert file not found: '" + CertificateFilename + "'");
                    CertificateFilename = "";
                }
            }
            if (!string.IsNullOrWhiteSpace(User) && OSUtils.IsUnix)
            {
                RMLog.Info($"-Run as user: '{User}'");
            }
            if (!string.IsNullOrWhiteSpace(RelayFilename)) {
                // If file doesn't exist, and it's relative, convert to absolute
                if (!File.Exists(RelayFilename) && !Path.IsPathRooted(RelayFilename)) {
                    RelayFilename = StringUtils.PathCombine(ProcessUtils.StartupPath, RelayFilename);
                }

                if (File.Exists(RelayFilename)) {
                    RMLog.Info("-Relay file: " + RelayFilename);
                } else {
                    RMLog.Error("-Relay file not found: '" + RelayFilename + "'");
                    RelayFilename = "";
                }
            }
            if (!string.IsNullOrWhiteSpace(RelayDeniedFilename)) {
                // If file doesn't exist, and it's relative, convert to absolute
                if (!File.Exists(RelayDeniedFilename) && !Path.IsPathRooted(RelayDeniedFilename)) {
                    RelayDeniedFilename = StringUtils.PathCombine(ProcessUtils.StartupPath, RelayDeniedFilename);
                }

                if (File.Exists(RelayDeniedFilename)) {
                    RMLog.Info("-Relay denied file: " + RelayDeniedFilename);
                } else {
                    RMLog.Error("-Relay denied file not found: '" + RelayDeniedFilename + "'");
                    RelayDeniedFilename = "";
                }
            }
            if (MaxIdleTimeInMinutes > 0) {
                RMLog.Info($"-Max idle time before disconnecting: {MaxIdleTimeInMinutes} minutes");
            } else {
                RMLog.Info("-Max idle time before disconnecting: DISABLED");
            }
            if (MaxSessionLengthInHours > 0) {
                RMLog.Info($"-Max session length before disconnecting: {MaxSessionLengthInHours} hours");
            } else {
                RMLog.Info("-Max session length before disconnecting: DISABLED");
            }
        }

        public double MaxIdleTimeInSeconds {
            get {
                return MaxIdleTimeInMinutes * 60;
            }
        }

        public double MaxSessionLengthInSeconds {
            get {
                return MaxSessionLengthInHours * 3600;
            }
        }
    }
}
