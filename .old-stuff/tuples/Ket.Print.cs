
namespace ket
{
    using System;
    using System.Linq;
    using System.Collections.Generic;
    using Microsoft.Quantum.Simulation.Core;

    public partial class Print
    {

        public class Native : Print
        {
            public Native(IOperationFactory m) : base(m)
            {
            }

            protected ICallable<Ket, IQArray<Int64>> Sample
            {
                get;
                set;
            }

            public override Func<Ket, QVoid> __Body__ => (ket) =>
            {
                var shots = 100;
                var histogram = new Dictionary<string, Int64>();

                for (var i = 0; i < shots; i++)
                {
                    var sample = Sample.Apply(ket).ToString();
                    if (histogram.ContainsKey(sample))
                    {
                        histogram[sample]++;
                    }
                    else
                    {
                        histogram[sample] = 1;
                    }
                }

                Console.WriteLine("Histogram:");
                foreach (var kvp in histogram.OrderBy(kvp => kvp.Key))
                {
                    var size = (int) kvp.Value * 100 / shots;
                    var bar = new String('*', size).PadRight(100);
                    Console.WriteLine($"{kvp.Key} -> {bar} ({kvp.Value})");
                }

                return QVoid.Instance;
            };

            public override void __Init__()
            {
                this.Sample = this.__Factory__.Get<ICallable<Ket, IQArray<Int64>>>(typeof(ket.Sample));
            }
        }
    }
}