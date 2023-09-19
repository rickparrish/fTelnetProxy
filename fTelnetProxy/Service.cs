﻿using System.ServiceProcess;

namespace RandM.fTelnetProxy
{
    public partial class Service : ServiceBase
    {
        private fTelnetProxy _fTelnetProxy;

        public Service()
        {
            InitializeComponent();
        }

        protected override void OnStart(string[] args)
        {
            _fTelnetProxy = new fTelnetProxy();
            _fTelnetProxy.Start();
        }

        protected override void OnStop()
        {
            _fTelnetProxy.Stop();
        }
    }
}
