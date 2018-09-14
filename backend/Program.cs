using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using System.Security.Cryptography.X509Certificates;
using System.Net;
using System.Reflection;

namespace backend
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var basePath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + "/publish";
            var config = new ConfigurationBuilder()
            .SetBasePath(basePath)
            .AddEnvironmentVariables()
            .AddJsonFile("certificate.json", optional: false, reloadOnChange: true)
            .AddJsonFile($"certificate.{Environment.GetEnvironmentVariable("ASPNETCORE_ENVIRONMENT")}.json", optional: true, reloadOnChange: true)
            .Build();

            var certificateSettings = config.GetSection("certificateSettings");
            string certificateFileName = certificateSettings.GetValue<string>("filename");
            string certificatePassword = certificateSettings.GetValue<string>("password");

            var certificate = new X509Certificate2($"{basePath}/{certificateFileName}", certificatePassword);

             WebHost.CreateDefaultBuilder(args)
                .UseEnvironment("Development")
                .UseKestrel(
                options =>
                    {
                        options.AddServerHeader = false;
                        options.Listen(IPAddress.Parse("192.168.1.39"), 5001, listenOptions =>
                        {
                            listenOptions.UseHttps(certificate);
                        });
                    }
                )
                .UseConfiguration(config)
                //.UseUrls("http://*:5000;https://*:5001;")
                .UseStartup<Startup>()
                .Build()
                .Run();
        }
           
    }
}
