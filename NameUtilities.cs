using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace XtoN
{
    internal static class NameUtilities
    {
        public static string LeftMostPart(NameSyntax name)
        {
            if (name is QualifiedNameSyntax qns)
            {
                return LeftMostPart(qns.Left);
            }

            if (name is SimpleNameSyntax sns)
            {
                return sns.Identifier.Text;
            }

            return "";
        }

        public static string FullName(NameSyntax name)
        {
            if (name is QualifiedNameSyntax qns)
            {
                return FullName(qns.Left) + "." + qns.Right.Identifier.Text;
            }

            if (name is SimpleNameSyntax sns)
            {
                return sns.Identifier.Text;
            }

            return "";
        }

        public static bool IsName(NameSyntax name, string fullName)
        {
            return name.IsEquivalentTo(SyntaxFactory.ParseName(fullName));
        }
    }
}