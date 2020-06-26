using System.IO;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;

namespace XtoN
{
    internal static class Program
    {
        private static async Task Main(string[] args)
        {
            foreach (string csFile in Directory.GetFiles(args[0], "*.cs", SearchOption.AllDirectories))
            {
                SourceText sourceText;
                await using (FileStream file = File.OpenRead(csFile))
                {
                    sourceText = SourceText.From(file);
                }

                SyntaxTree tree = CSharpSyntaxTree.ParseText(sourceText, path: csFile);
                SyntaxNode newRoot = new XUnitToNUnitRewrite().Visit(await tree.GetRootAsync());
                SyntaxTree newTree = tree.WithRootAndOptions(newRoot, tree.Options);
                SourceText newText = await newTree.GetTextAsync();
                await using (FileStream file = File.Create(csFile))
                await using (var writer = new StreamWriter(file, new UTF8Encoding(false)))
                {
                    newText.Write(writer);
                }
            }
        }
    }
}