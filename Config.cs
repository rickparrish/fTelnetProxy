using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.Text;

namespace RandM.fTelnetProxy
{
    public class Config : ConfigHelper
    {
        public string CertFilename { get; set; }
        public string CertPassword { get; set; }
        public int ListenPort { get; set; }
        public string RelayFilename { get; set; }
        public string TargetHostname { get; set; }
        public int TargetPort { get; set; }

        static public Config Default = new Config();

        public Config(): base()
        {
            CertFilename = "";
            CertPassword = "";
            ListenPort = 1123;
            RelayFilename = "";
            TargetHostname = "localhost";
            TargetPort = 23;

            base.Load();
        }
    }
}
