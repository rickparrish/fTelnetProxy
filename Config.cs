using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace RandM.fTelnetProxy
{
    public class Config : ConfigHelper
    {
        public string CertFilename { get; set; }
        public string CertPassword { get; set; }
        public int ListenPort { get; set; }
        public LogLevel LogLevel { get; set; }
        public string RelayFilename { get; set; }
        public string TargetHostname { get; set; }
        public int TargetPort { get; set; }

        static public Config Default = new Config();

        public Config()
            : base(ConfigSaveLocation.Relative)
        {
            CertFilename = "";
            CertPassword = "";
            ListenPort = 1123;
            LogLevel = LogLevel.Info;
            RelayFilename = "";
            TargetHostname = "localhost";
            TargetPort = 23;
        }

        public new void Load()
        {
            // Try to load, and save a new file if load failed
            if (!base.Load()) base.Save();

            RMLog.Level = LogLevel;

            // Output the settings being used
            RMLog.Info("Using settings from " + base.FileName);
            RMLog.Info("-Listen port...." + ListenPort.ToString());
            RMLog.Info("-Target server.." + TargetHostname + ":" + TargetPort.ToString());
            RMLog.Info("-Log level......" + LogLevel.ToString());
            if (CertFilename != "")
            {
                if (File.Exists(CertFilename))
                {
                    RMLog.Info("-Cert file......" + CertFilename);
                    if (CertPassword == "")
                    {
                        RMLog.Info("-Cert password..none");
                    }
                    else
                    {
                        RMLog.Info("-Cert password..yes (hidden)");
                    }
                }
                else
                {
                    RMLog.Error("-Cert file not found: '" + CertFilename + "'");
                    CertFilename = "";
                }

            }
            if (RelayFilename != "")
            {
                if (File.Exists(RelayFilename))
                {
                    RMLog.Info("-Relay file....." + RelayFilename);
                }
                else
                {
                    RMLog.Error("-Relay file not found: '" + RelayFilename + "'");
                    RelayFilename = "";
                }
            }
        }
    }
}
