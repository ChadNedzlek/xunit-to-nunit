using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using CSharpExtensions = Microsoft.CodeAnalysis.CSharp.CSharpExtensions;

namespace XtoN
{
    public class XUnitToNUnitRewrite : CSharpSyntaxRewriter
    {
        private bool _fileHasFacts;

        public override SyntaxNode VisitCompilationUnit(CompilationUnitSyntax node)
        {
            if (!node.Usings.Any(u => NameUtilities.FullName(u.Name).StartsWith("Xunit")))
            {
                // This doesn't have any Xunit using declarations, it's probably some other framework
                return node;
            }

            _fileHasFacts = false;
            node = (CompilationUnitSyntax) base.VisitCompilationUnit(node);

            var firstTrivia = node?.Usings.FirstOrDefault()?.GetLeadingTrivia() ?? TriviaList();

            var usingDirectives  = node.Usings.Where(u => !NameUtilities.IsName(u.Name, "Xunit") && !NameUtilities.IsName(u.Name, "Xunit.Abstractions")).ToList();

            if (_fileHasFacts)
            {
                UsingDirectiveSyntax nUnitUsing = UsingDirective(ParseName("NUnit.Framework").WithLeadingTrivia(Space));
                usingDirectives.Add(nUnitUsing);
                usingDirectives = SortUsing(usingDirectives);
            }

            if (usingDirectives.Count != 0)
            {
                usingDirectives[0] = usingDirectives[0].WithLeadingTrivia(firstTrivia);
            }

            node = node.WithUsings(List(usingDirectives));

            return node;
        }

        private List<UsingDirectiveSyntax> SortUsing(IReadOnlyList<UsingDirectiveSyntax> usingDirectives)
        {
            int NameSortingComparison(UsingDirectiveSyntax a, UsingDirectiveSyntax b)
            {
                string aLeft = NameUtilities.LeftMostPart(a.Name);
                string bLeft = NameUtilities.LeftMostPart(b.Name);
                string aFull = NameUtilities.FullName(a.Name);
                string bBull = NameUtilities.FullName(b.Name);
                if (aLeft == "System")
                {
                    if (bLeft == "System")
                    {
                        return StringComparer.Ordinal.Compare(aFull, bBull);
                    }

                    return -1;
                }


                if (bLeft == "System")
                {
                    return 1;
                }

                return StringComparer.Ordinal.Compare(aFull, bBull);
            }

            List<UsingDirectiveSyntax> normal = usingDirectives
                .Where(u => u.Alias == null && u.StaticKeyword == default)
                .Select(u => u.WithoutTrivia().WithTrailingTrivia(CarriageReturnLineFeed))
                .ToList();
            normal.Sort(NameSortingComparison);
            List<UsingDirectiveSyntax> aliases = usingDirectives.Where(u => u.Alias != null)
                .Select(u => u.WithoutTrivia().WithTrailingTrivia(CarriageReturnLineFeed))
                .ToList();
            aliases.Sort(NameSortingComparison);
            List<UsingDirectiveSyntax> statics = usingDirectives.Where(u => u.StaticKeyword != default)
                .Select(u => u.WithoutTrivia().WithTrailingTrivia(CarriageReturnLineFeed))
                .ToList();
            statics.Sort(NameSortingComparison);

            if (normal.Count != 0 && (aliases.Count != 0 || statics.Count != 0))
            {
                normal[^1] = normal[^1].WithTrailingTrivia(CarriageReturnLineFeed, CarriageReturnLineFeed);
            }

            if (aliases.Count != 0 && statics.Count != 0)
            {
                aliases[^1] = aliases[^1].WithTrailingTrivia(CarriageReturnLineFeed, CarriageReturnLineFeed);
            }

            return normal.Concat(aliases).Concat(statics).ToList();
        }

        private class ClassTracker
        {
            public int TestCount;
            public bool HasConstructor;
            public bool HasDispose;
        }

        private readonly Stack<ClassTracker> _classState = new Stack<ClassTracker>();
        public override SyntaxNode VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            if (node.Identifier.Text == "BuildController20190116Tests")
            {
            }

            _classState.Push(new ClassTracker());

            ClassDeclarationSyntax newNode = (ClassDeclarationSyntax) base.VisitClassDeclaration(node);

            var state = _classState.Pop();

            if (state.TestCount == 0)
            {
                return node;
            }

            SeparatedSyntaxList<AttributeSyntax> attributeList = SeparatedList(
                new[]
                {
                    Attribute(IdentifierName("TestFixture"))
                }
            );

            if (state.TestCount > 1 && (state.HasConstructor || state.HasDispose))
            {
                // If there are multiple tests and there is setup/teardown
                // it's almost guaranteed it was not meant to run in parallel
                attributeList = attributeList.Add(Attribute(IdentifierName("NonParallelizable")).WithLeadingTrivia(Space));
            }

            SyntaxList<AttributeListSyntax> list = newNode.AttributeLists.Add(
                AttributeList(
                        attributeList
                    )
                    .WithLeadingTrivia(newNode.GetLeadingTrivia())
                    .WithTrailingTrivia(CarriageReturnLineFeed)
            );
            newNode = newNode.WithAttributeLists(list);

            return newNode;
        }

        public override SyntaxNode VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            _classState.Peek().HasConstructor = true;
            return MethodDeclaration(
                attributeLists: List(
                    new[]
                    {
                        AttributeList(
                                SeparatedList(new[]
                                    {
                                        Attribute(IdentifierName("SetUp"))
                                    }
                                )
                            )
                            .WithLeadingTrivia(node.GetLeadingTrivia())
                    }
                ),
                modifiers: TokenList(Token(SyntaxKind.PublicKeyword).WithLeadingTrivia(node.GetLeadingTrivia())),
                returnType: ParseTypeName("void").WithLeadingTrivia(Space),
                explicitInterfaceSpecifier: null,
                identifier: Identifier($"{node.Identifier.Text}_SetUp").WithLeadingTrivia(Space),
                typeParameterList: null,
                ParameterList().WithTrailingTrivia(CarriageReturnLineFeed),
                List<TypeParameterConstraintClauseSyntax>(),
                body: node.Body,
                expressionBody: null
            );
        }

        public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            Predicate<AttributeSyntax> IsXunitAttribute(string value)
            {
                return a =>
                {
                    string fullName = NameUtilities.FullName(a.Name);
                    return fullName == value || fullName == $"Xunit.{value}";
                };
            }

            SyntaxTriviaList leadingTrivia =
                node.AttributeLists.FirstOrDefault()?.GetLeadingTrivia() ?? new SyntaxTriviaList();
            SyntaxTriviaList trailingTrivia =
                node.AttributeLists.LastOrDefault()?.GetTrailingTrivia() ?? new SyntaxTriviaList();
            var leadBetween = new SyntaxTriviaList();
            var trailingBetween = new SyntaxTriviaList();
            if (node.AttributeLists.Count > 1)
            {
                leadBetween = node.AttributeLists[1].GetLeadingTrivia();
                trailingBetween = node.AttributeLists[0].GetTrailingTrivia();
            }

            bool changes = false;
            List<AttributeSyntax> allAttributes = node.AttributeLists.SelectMany(l => l.Attributes).ToList();
            if (allAttributes.Exists(IsXunitAttribute("Fact")))
            {
                allAttributes.RemoveAll(IsXunitAttribute("Fact"));
                allAttributes.Add(Attribute(IdentifierName("Test")));
                changes = true;
                FoundTest();
            }

            if (allAttributes.Exists(IsXunitAttribute("Theory")))
            {
                List<AttributeSyntax> inlineData = allAttributes.FindAll(IsXunitAttribute("InlineData"));
                List<AttributeSyntax> memberData = allAttributes.FindAll(IsXunitAttribute("MemberData"));
                allAttributes.AddRange(inlineData.Select(i => Attribute(IdentifierName("TestCase"), i.ArgumentList)));
                allAttributes.AddRange(memberData.Select(m =>
                    Attribute(IdentifierName("TestCaseSource"), m.ArgumentList)));
                allAttributes.RemoveAll(IsXunitAttribute("Theory"));
                allAttributes.RemoveAll(IsXunitAttribute("InlineData"));
                allAttributes.RemoveAll(IsXunitAttribute("MemberData"));
                changes = true;
                FoundTest();
            }

            if (node.Identifier.Text == "Dispose" && node.Modifiers.All(m => m.Kind() != SyntaxKind.OverrideKeyword))
            {
                allAttributes.Add(Attribute(IdentifierName("TearDown")));
                leadingTrivia = node.GetLeadingTrivia();
                changes = true;
                _classState.Peek().HasDispose = true;
            }

            node = (MethodDeclarationSyntax) base.VisitMethodDeclaration(node);

            if (changes)
            {
                List<AttributeListSyntax> attributeLists = allAttributes.Select(a =>
                        AttributeList(
                                SeparatedList(
                                    new[]
                                    {
                                        a
                                    }
                                )
                            )
                            .WithLeadingTrivia(leadBetween)
                            .WithTrailingTrivia(trailingBetween)
                    )
                    .ToList();
                attributeLists[0] = attributeLists[0].WithLeadingTrivia(leadingTrivia);
                attributeLists[^1] = attributeLists[^1].WithTrailingTrivia(trailingTrivia);
                node = node.WithAttributeLists(List(attributeLists));
            }

            return node;
        }

        public override SyntaxNode VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            var attempts = new Func<ExpressionStatementSyntax, SyntaxNode>[]
            {
                AssertExpressionBuilder.TryAssertThrowsAsyncStatement,
                AssertExpressionBuilder.TryAssertThrowsStatement,
                AssertExpressionBuilder.TryAssertThrowsAnyStatement,
                base.VisitExpressionStatement
            };

            foreach (Func<ExpressionStatementSyntax, SyntaxNode> attempt in attempts)
            {
                SyntaxNode newNode = attempt(node);
                if (newNode != null)
                {
                    return newNode;
                }
            }

            return null;
        }

        public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            if (node.Expression is MemberAccessExpressionSyntax memberAccess)
            {
                if (memberAccess.Expression is NameSyntax name && AssertExpressionBuilder.IsAssert(name))
                {
                    return AssertExpressionBuilder.TranslateAssert(node, memberAccess).WithTriviaFrom(node);
                }
            }

            return base.VisitInvocationExpression(node);
        }


        private void FoundTest()
        {
            _classState.Peek().TestCount++;
            _fileHasFacts = true;
        }
    }
}