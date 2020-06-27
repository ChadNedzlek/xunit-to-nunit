// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace XtoN
{
    internal static class AssertExpressionBuilder
    {
        public static SyntaxNode TryAssertThrowsAsyncStatement(ExpressionStatementSyntax node)
        {
            if (node.Expression is AwaitExpressionSyntax awaitExpression &&
                awaitExpression.Expression is InvocationExpressionSyntax invocation &&
                invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                memberAccess.Expression is NameSyntax name &&
                IsAssert(name) &&
                memberAccess.Name.Identifier.Text == "ThrowsAsync" &&
                memberAccess.Name is GenericNameSyntax throwsAsyncName)
            {
                return ExpressionStatement(
                    Ex.Static("Assert")
                        .Call("That",
                            invocation.ArgumentList.Arguments[0].Expression,
                            Ex.Static("Throws").CallGeneric("TypeOf", throwsAsyncName.TypeArgumentList)
                        )
                );
            }

            return null;
        }

        public static SyntaxNode TryAssertThrowsStatement(ExpressionStatementSyntax node)
        {
            if (node.Expression is InvocationExpressionSyntax invocation &&
                invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                memberAccess.Expression is NameSyntax name &&
                IsAssert(name) &&
                memberAccess.Name.Identifier.Text == "Throws" &&
                memberAccess.Name is GenericNameSyntax throwsAsyncName)
            {
                return ExpressionStatement(
                    Ex.Static("Assert")
                        .Call("That",
                            invocation.ArgumentList.Arguments[0].Expression,
                            Ex.Static("Throws").CallGeneric("TypeOf", throwsAsyncName.TypeArgumentList)
                        )
                );
            }

            return null;
        }

        public static SyntaxNode TryAssertThrowsAnyStatement(ExpressionStatementSyntax node)
        {
            if (node.Expression is InvocationExpressionSyntax invocation &&
                invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                memberAccess.Expression is NameSyntax name &&
                IsAssert(name) &&
                memberAccess.Name.Identifier.Text == "ThrowsAny" &&
                memberAccess.Name is GenericNameSyntax throwsAsyncName)
            {
                return ExpressionStatement(
                    Ex.Static("Assert")
                        .Call("That",
                            invocation.ArgumentList.Arguments[0].Expression,
                            Ex.Static("Throws").CallGeneric("InstanceOf", throwsAsyncName.TypeArgumentList)
                        )
                );
            }

            return null;
        }

        public static bool IsAssert(NameSyntax name)
        {
            string fullName = NameUtilities.FullName(name);
            return fullName == "Assert" || fullName == "Xunit.Assert";
        }

        public static SyntaxNode TranslateAssert(InvocationExpressionSyntax node, MemberAccessExpressionSyntax memberAccess)
        {
            switch (memberAccess.Name.Identifier.Text)
            {
                case "Equal":
                    return AssertEquals(node);
                case "NotEqual":
                    return AssertNotEquals(node);
                case "Empty":
                    return AssertThat(node.ArgumentList.Arguments[0].Expression, "Is", "Empty");
                case "Single":
                    return AssertThat(node.ArgumentList.Arguments[0].Expression, "Has", "One", "Items");
                case "Same":
                    return AssertThat(node.ArgumentList.Arguments[1].Expression,
                        "Is",
                        "SameAs",
                        node.ArgumentList.Arguments[0].Expression);
                case "NotSame":
                    return AssertThat(node.ArgumentList.Arguments[1].Expression,
                        "Is",
                        "Not",
                        "SameAs",
                        node.ArgumentList.Arguments[0].Expression);
                case "True":
                case "False":
                    return AssertThat(node.ArgumentList.Arguments[0].Expression,
                        "Is",
                        memberAccess.Name.Identifier.Text);
                case "NotNull":
                    return AssertThat(node.ArgumentList.Arguments[0].Expression, "Is", "Not", "Null");
                case "Null":
                    return AssertThat(node.ArgumentList.Arguments[0].Expression, "Is", "Null");
                case "StartsWith":
                    return AssertThat(node.ArgumentList.Arguments[1].Expression,
                        "Does",
                        "StartWith",
                        node.ArgumentList.Arguments[0].Expression);
                case "IsType":
                    return AssertIsType(node, memberAccess, "TypeOf");
                case "IsAssignableFrom":
                    return AssertIsType(node, memberAccess, "InstanceOf");
                case "Throws":
                    return AssertThrows(node, memberAccess, "Throws");
                case "ThrowsAny":
                    return AssertThrows(node, memberAccess, "Catch");
                case "ThrowsAsync":
                    return AssertThrows(node, memberAccess, "ThrowsAsync");
                case "ThrowsAnyAsync":
                    return AssertThrows(node, memberAccess, "CatchAsync");
                case "Contains":
                    return AssertThat(node.ArgumentList.Arguments[1].Expression,
                        "Contains",
                        "Item",
                        node.ArgumentList.Arguments[0].Expression);
                case "DoesNotContain":
                    return AssertThat(node.ArgumentList.Arguments[1].Expression,
                        "Does",
                        "Not",
                        "Contain",
                        node.ArgumentList.Arguments[0].Expression);
                case "Collection":
                    return AssertCollection(node);
                default:
                    throw new NotSupportedException(
                        $"Could not translate 'Assert.{memberAccess.Name.Identifier.Text}'");
            }
        }

        private static ExpressionSyntax AssertCollection(InvocationExpressionSyntax node)
        {
            Ex basic = AssertCollectionExpression(node);

            return Ex.Static("Assert")
                .Call("That",
                    node.ArgumentList.Arguments[0].Expression,
                    basic
                );
        }

        public static Ex AssertCollectionExpression(InvocationExpressionSyntax node)
        {
            Ex basic = Ex.Static("Has").Call("Exactly", node.ArgumentList.Arguments.Count - 1).Dot("Items");
            foreach (ArgumentSyntax ex in node.ArgumentList.Arguments.Skip(1))
            {
                basic = basic.Dot("And").Dot("One").Call("Matches", AssertDelegateToConstraint(ex.Expression));
            }

            return basic;
        }

        private static ExpressionSyntax AssertDelegateToConstraint(ExpressionSyntax expression)
        {
            if (!(expression is LambdaExpressionSyntax lambda))
            {
                throw new NotSupportedException("Cannot translate Assert.Collection without lambda");
            }

            string paramName;
            if (lambda is SimpleLambdaExpressionSyntax simple)
            {
                paramName = simple.Parameter.Identifier.Text;
            }
            else if (lambda is ParenthesizedLambdaExpressionSyntax paren)
            {
                if (paren.ParameterList.Parameters.Count != 1)
                {
                    throw new NotSupportedException("Cannot translate Assert.Collection lambdas must have 1 parameter");
                }

                paramName = paren.ParameterList.Parameters[0].Identifier.Text;
            }
            else
            {
                throw new Exception($"Unexpected lambda of type '{lambda.Kind()}'");
            }

            if (lambda.Block == null)
            {
                throw new NotSupportedException("Cannot translate Assert.Collection lambdas must be statement lambdas");
            }

            ExpressionSyntax ex = null;

            foreach (StatementSyntax statement in lambda.Block.Statements)
            {
                if (statement is ExpressionStatementSyntax expressionStatement &&
                    expressionStatement.Expression is InvocationExpressionSyntax invocation &&
                    invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                    memberAccess.Expression is SimpleNameSyntax expressionName &&
                    expressionName.Identifier.Text == "Assert")
                {
                    SimpleNameSyntax memberAccessName = memberAccess.Name;
                    ex = GetConstrainFromAssert(invocation, paramName, memberAccessName.Identifier.Text, ex);
                }
                else
                {
                    throw new NotSupportedException(
                        "Cannot translate Assert.Collection lambdas must be statements that are all Assert expressions");
                }
            }

            if (ex == null)
            {
                throw new NotSupportedException("Cannot translate Assert.Collection lambdas with empty bodies");
            }

            return ex;
        }


        private static ExpressionSyntax GetConstrainFromAssert(
            InvocationExpressionSyntax invocation,
            string paramName,
            string memberName,
            ExpressionSyntax leftPart)
        {
            Ex ConvertMemberExpression(MemberAccessExpressionSyntax e, ExpressionSyntax l)
            {
                Ex left;
                if (e.Expression is MemberAccessExpressionSyntax m)
                {
                    left = ConvertMemberExpression(m, l);
                }
                else if (e.Expression is SimpleNameSyntax n)
                {
                    if (n.Identifier.Text != paramName)
                    {
                        return default;
                    }

                    left = l == null ? Ex.Static("Has") : Ex.From(l).Dot("And");
                }
                else
                {
                    return default;
                }

                return left.Call("Property", Ex.Literal(e.Name.Identifier.Text));
            }

            Ex ConvertToConstraint(ExpressionSyntax e, ExpressionSyntax l)
            {
                if (e is MemberAccessExpressionSyntax m)
                {
                    return ConvertMemberExpression(m, l);
                }

                if (e is SimpleNameSyntax)
                {
                    if (l == null)
                    {
                        return Ex.Static("Is");
                    }

                    return Ex.From(l).Dot("And");
                }

                return default;
            }

            switch (memberName)
            {
                case "Equal":
                    Ex constraint = ConvertToConstraint(invocation.ArgumentList.Arguments[1].Expression, leftPart);
                    ExpressionSyntax expected = invocation.ArgumentList.Arguments[0].Expression;
                    if (constraint.IsNull)
                    {
                        constraint = ConvertToConstraint(invocation.ArgumentList.Arguments[0].Expression, leftPart);
                        expected = invocation.ArgumentList.Arguments[0].Expression;
                    }

                    if (constraint.IsNull)
                    {
                        throw new NotSupportedException("Could not translate CollectionAssert: " +
                            invocation.SyntaxTree.GetLineSpan(invocation.Span));
                    }

                    return constraint.Call("EqualTo", expected);
                case "Null":
                    if (leftPart == null)
                    {
                        return Ex.Static("Is").Dot("Null");
                    }
                    else
                    {
                        return Ex.From(leftPart).Dot("Null");
                    }
                case "Collection":
                    Ex member = ConvertToConstraint(invocation.ArgumentList.Arguments[0].Expression, leftPart);
                    return member.Call("Matches", AssertCollectionExpression(invocation));
                default:
                    throw new NotSupportedException($"Cannot translate Collection with Assert.{memberName}");
            }
        }

        private static SyntaxNode Assert(SimpleNameSyntax name, ExpressionSyntax argument)
        {
            return InvocationExpression(MemberAccessExpression(
                    IdentifierName("Assert"),
                    name
                ),
                ArgumentList(
                    SeparatedList(
                        new[]
                        {
                            Argument(argument)
                        }
                    )
                )
            );
        }

        private static SyntaxNode AssertThrows(
            InvocationExpressionSyntax node,
            MemberAccessExpressionSyntax memberAccess,
            string assertMethodName)
        {
            SimpleNameSyntax typeName;
            if (memberAccess.Name is GenericNameSyntax gen)
            {
                typeName = GenericName(Identifier(assertMethodName), gen.TypeArgumentList);
            }
            else
            {
                throw new NotSupportedException("Non generic IsType is not supported");
            }

            return Assert(typeName, node.ArgumentList.Arguments[0].Expression);
        }

        private static SyntaxNode AssertEquals(InvocationExpressionSyntax node)
        {
            ExpressionSyntax counted = TryGetCountedExpression(node);

            if (counted != null)
            {
                return AssertThat(counted, "Has", "Exactly", node.ArgumentList.Arguments[0].Expression, "Items");
            }

            return AssertThat(node.ArgumentList.Arguments[1].Expression,
                "Is",
                "EqualTo",
                node.ArgumentList.Arguments[0].Expression);
        }

        private static SyntaxNode AssertNotEquals(InvocationExpressionSyntax node)
        {
            ExpressionSyntax counted = TryGetCountedExpression(node);

            if (counted != null)
            {
                return AssertThat(counted,
                    "Is",
                    "Not",
                    "Exactly",
                    node.ArgumentList.Arguments[0].Expression,
                    "Items");
            }

            return AssertThat(node.ArgumentList.Arguments[1].Expression,
                "Is",
                "EqualTo",
                node.ArgumentList.Arguments[0].Expression);
        }

        private static ExpressionSyntax TryGetCountedExpression(InvocationExpressionSyntax node)
        {
            ExpressionSyntax counted = null;
            ExpressionSyntax actualExpression = node.ArgumentList.Arguments[1].Expression;
            if (actualExpression is MemberAccessExpressionSyntax maybeCountProperty &&
                (maybeCountProperty.Name.Identifier.Text == "Count" ||
                    maybeCountProperty.Name.Identifier.Text == "Length"))
            {
                counted = maybeCountProperty.Expression;
            }
            else if (actualExpression is InvocationExpressionSyntax maybeCountMethod)
            {
                if (maybeCountMethod.ArgumentList.Arguments.Count == 0 &&
                    maybeCountMethod.Expression is MemberAccessExpressionSyntax maybeCountMethodMember &&
                    maybeCountMethodMember.Name.Identifier.Text == "Count")
                {
                    counted = maybeCountMethodMember.Expression;
                }
            }

            return counted;
        }

        private static ExpressionSyntax AssertThat(
            ExpressionSyntax argumentExpression,
            ExpressionSyntax constraintExpression)
        {
            return Ex.Static("Assert")
                .Call("That",
                    argumentExpression,
                    constraintExpression.WithLeadingTrivia(Space)
                );
        }

        private static SyntaxNode AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            string constraintMember,
            ExpressionSyntax constraintArgument,
            string constraintModifier)
        {
            MemberAccessExpressionSyntax memberAccessExpressionSyntax = MemberAccessExpression(
                InvocationExpression(MemberAccessExpression(
                        IdentifierName(constraintClass),
                        IdentifierName(constraintMember)
                    ),
                    ArgumentList(
                        SeparatedList(
                            new[]
                            {
                                Argument(
                                    constraintArgument
                                )
                            }
                        )
                    )
                ),
                IdentifierName(constraintModifier)
            );

            return AssertThat(actual,
                memberAccessExpressionSyntax
            );
        }

        private static SyntaxNode AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            string constraintModifier1,
            string constraintMember,
            ExpressionSyntax constraintArgument,
            string constraintModifier2)
        {
            return AssertThat(actual,
                MemberAccessExpression(
                    InvocationExpression(MemberAccessExpression(MemberAccessExpression(
                                IdentifierName(constraintClass),
                                IdentifierName(constraintModifier1)
                            ),
                            IdentifierName(constraintMember)
                        ),
                        ArgumentList(
                            SeparatedList(
                                new[]
                                {
                                    Argument(
                                        constraintArgument
                                    )
                                }
                            )
                        )
                    ),
                    IdentifierName(constraintModifier2)
                )
            );
        }

        private static SyntaxNode AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            string constraintMember,
            ExpressionSyntax constraintArgument)
        {
            InvocationExpressionSyntax memberAccessExpressionSyntax = InvocationExpression(
                MemberAccessExpression(
                    IdentifierName(constraintClass),
                    IdentifierName(constraintMember)
                ),
                ArgumentList(
                    SeparatedList(
                        new[]
                        {
                            Argument(
                                constraintArgument
                            )
                        }
                    )
                )
            );

            return AssertThat(actual,
                memberAccessExpressionSyntax
            );
        }

        private static SyntaxNode AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            string constrainModifier,
            string constraintMember,
            ExpressionSyntax constraintArgument)
        {
            InvocationExpressionSyntax memberAccessExpressionSyntax = InvocationExpression(
                MemberAccessExpression(MemberAccessExpression(
                        IdentifierName(constraintClass),
                        IdentifierName(constrainModifier)
                    ),
                    IdentifierName(constraintMember)
                ),
                ArgumentList(
                    SeparatedList(
                        new[]
                        {
                            Argument(
                                constraintArgument
                            )
                        }
                    )
                )
            );

            return AssertThat(actual,
                memberAccessExpressionSyntax
            );
        }

        private static SyntaxNode AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            string constraintMember)
        {
            return AssertThat(actual, constraintClass, IdentifierName(constraintMember));
        }

        private static ExpressionSyntax AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            SimpleNameSyntax constraintMember)
        {
            MemberAccessExpressionSyntax memberAccessExpressionSyntax = MemberAccessExpression(
                IdentifierName(constraintClass),
                constraintMember
            );

            return AssertThat(actual,
                memberAccessExpressionSyntax
            );
        }

        private static SyntaxNode AssertThat(
            ExpressionSyntax actual,
            string constraintClass,
            string constraintModifier,
            string constraintMember)
        {
            MemberAccessExpressionSyntax memberAccessExpressionSyntax = MemberAccessExpression(MemberAccessExpression(
                    IdentifierName(constraintClass),
                    IdentifierName(constraintModifier)
                ),
                IdentifierName(constraintMember)
            );

            return AssertThat(actual,
                memberAccessExpressionSyntax
            );
        }

        private static MemberAccessExpressionSyntax MemberAccessExpression(
            ExpressionSyntax expression,
            SimpleNameSyntax name)
        {
            return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression, name);
        }

        private static SyntaxNode AssertIsType(
            InvocationExpressionSyntax node,
            MemberAccessExpressionSyntax memberAccess,
            string memberName)
        {
            SimpleNameSyntax memberNameSyntax;
            if (memberAccess.Name is GenericNameSyntax gen)
            {
                memberNameSyntax = GenericName(Identifier(memberName), gen.TypeArgumentList);
            }
            else
            {
                throw new NotSupportedException($"Non generic {memberName} is not supported");
            }

            return AssertThat(node.ArgumentList.Arguments[0].Expression, "Is", memberNameSyntax);
        }
    }
}