using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace XtoN
{
    /// <summary>
    /// Helper methods for generating expressions, since Roslyn code can be a bit verbose
    /// </summary>
    public readonly struct Ex
    {
        private readonly ExpressionSyntax _node;

        private Ex(ExpressionSyntax node)
        {
            _node = node;
        }

        public bool IsNull => _node == null;

        public Ex Call(string method, params ExpressionSyntax[] args)
        {
            return new Ex(
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        _node,
                        IdentifierName(method)),
                    ArgumentList(
                        SeparatedList(
                            args.Select(Argument)
                        )
                    )
                )
            );
        }

        public Ex Call(string method, params int[] args)
        {
            return new Ex(
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        _node,
                        IdentifierName(method)),
                    ArgumentList(
                        SeparatedList(
                            args.Select(i => Argument(Literal((int) i)))
                        )
                    )
                )
            );
        }

        public Ex CallGeneric(string method, TypeArgumentListSyntax types, params ExpressionSyntax[] args)
        {
            return new Ex(
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        _node,
                        GenericName(
                            Identifier(method),
                            types
                        )
                    ),
                    ArgumentList(
                        SeparatedList(
                            args.Select(Argument)
                        )
                    )
                )
            );
        }

        public Ex Dot(string propertyOrField)
        {
            return new Ex(
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    _node,
                    IdentifierName(propertyOrField)
                )
            );
        }

        public static Ex Static(string name)
        {
            return new Ex(IdentifierName(name));
        }

        public static Ex From(ExpressionSyntax syntax)
        {
            return new Ex(syntax);
        }

        public static implicit operator ExpressionSyntax(Ex ex)
        {
            return ex._node;
        }

        public static ExpressionSyntax Literal(string value)
        {
            return LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(value));
        }

        private static ExpressionSyntax Literal(int value)
        {
            return LiteralExpression(SyntaxKind.NullLiteralExpression, SyntaxFactory.Literal(value));
        }
    }
}