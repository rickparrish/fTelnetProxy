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
        public string RelayFilename { get; set; }
        public int RLoginPort { get; set; }
        public string TargetHostname { get; set; }
        public int TargetPort { get; set; }

        static public Config Default = new Config();

        public Config()
            : base(ConfigSaveLocation.Relative) {
            Certificate = null;
            CertificateFilename = "";
            CertificatePassword = "";
            ListenPort = 80; // TODOX ip6tables doesn't support port forwarding, so might need option to listen on multiple ports
            LogLevel = LogLevel.Info;
            RelayFilename = "";
            RLoginPort = 513;
            TargetHostname = "localhost";
            TargetPort = 23;
        }

        public X509Certificate2 Certificate { get; set; }

        public new void Load() {
            // Try to load, and save a new file if load failed
            if (!base.Load()) base.Save();

            RMLog.Level = LogLevel;

            // Output the settings being used
            RMLog.Info("Using settings from " + base.FileName);
            RMLog.Info("-Listen port...." + ListenPort.ToString());
            if (TargetPort > 0) {
                RMLog.Info("-Telnet target.." + TargetHostname + ":" + TargetPort.ToString());
            } else {
                RMLog.Info("-Telnet target..DISABLED");
            }
            if (RLoginPort > 0) {
                RMLog.Info("-RLogin target.." + TargetHostname + ":" + RLoginPort.ToString());
            } else {
                RMLog.Info("-RLogin target..DISABLED");
            }
            RMLog.Info("-Log level......" + LogLevel.ToString());
            if (CertificateFilename != "") {
                // If file doesn't exist, and it's relative, convert to absolute
                if (!File.Exists(CertificateFilename) && !Path.IsPathRooted(CertificateFilename)) {
                    CertificateFilename = StringUtils.PathCombine(ProcessUtils.StartupPath, CertificateFilename);
                }

                if (File.Exists(CertificateFilename)) {
                    RMLog.Info("-Cert file......" + CertificateFilename);
                    if (CertificatePassword == "") {
                        RMLog.Info("-Cert password..none");
                    } else {
                        RMLog.Info("-Cert password..yes (hidden)");
                    }
                } else {
                    RMLog.Error("-Cert file not found: '" + CertificateFilename + "'");
                    CertificateFilename = "";
                }
            }
            if (RelayFilename != "") {
                // If file doesn't exist, and it's relative, convert to absolute
                if (!File.Exists(RelayFilename) && !Path.IsPathRooted(RelayFilename)) {
                    RelayFilename = StringUtils.PathCombine(ProcessUtils.StartupPath, RelayFilename);
                }

                if (File.Exists(RelayFilename)) {
                    RMLog.Info("-Relay file....." + RelayFilename);
                } else {
                    RMLog.Error("-Relay file not found: '" + RelayFilename + "'");
                    RelayFilename = "";
                }
            }
        }
    }
}
