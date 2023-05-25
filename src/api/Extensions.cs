namespace aleph.server;

using static aleph.kets;

public static class KetValueExtensions
{
    public static string Label(this KetValue ket)
    {
        if (ket.Expression.IsLiteral)
        {
            var literal = (Expression.Literal)ket.Expression;
            return $"literal (width: {literal.width})";
        }
        else if (ket.Expression.IsConstant)
        {
            var c = (Expression.Constant)ket.Expression;
            return $"constant ({c.value})";
        }
        else if (ket.Expression.IsMap)
        {
            var map = (Expression.Map)ket.Expression;
            return $"map ({map.op.Label()})";
        }
        else if (ket.Expression.IsWhere)
        {
            var w = (Expression.Where)ket.Expression;
            return $"where (op: {w.clause.Label()})";
        }

        throw new NotImplementedException();
    }

    public static GraphNode[] Dependencies(this KetValue ket, QuantumGraph graph)
    {
        if (ket.Expression.IsLiteral || ket.Expression.IsConstant)
        {
            return new GraphNode[0];
        }
        else if (ket.Expression.IsMap)
        {
            var map = (Expression.Map)ket.Expression;
            return map.args
                .Select((ket, idx) => new GraphNode(ket.Id, graph))
                .ToArray();
        }
        else if (ket.Expression.IsWhere)
        {
            var where = (Expression.Where)ket.Expression;
            return where.args
                .Select((ket, idx) => new GraphNode(ket.Id, graph))
                .Append(new GraphNode(where.target.Id, graph))
                .ToArray();
        }

        throw new NotImplementedException();
    }
}

public static class OperatorExtensions
{
    public static string Label(this Operator op) =>
            op.IsNot ?
                "not" :
            op.IsLessThanEquals ?
                "<=" :
            op.IsEq ?
                "==" :
            op.IsAdd ?
                $"+, w={((Operator.Add)op).width}" :
            op.IsMultiply ?
                "*, w={((Operator.Multiply) op).width}" :
            op.IsIf ?
                "if" :
            op.IsId ?
                "id" :
            op.IsIn ?
                "in" :
            op.IsGreaterThan ?
                ">" :
            op.IsAnd ?
                "and" :
            op.IsOr ?
                "or" :
            throw new NotImplementedException();

    public static Operator Parse(string label) =>
        "eq" == label ?
            Operator.Eq :
        "not" == label ?
            Operator.Not :
        "lte" == label ?
            Operator.LessThanEquals :
        "if" == label ?
            Operator.If :
        "id" == label ?
            Operator.Id :
        "gt" == label ?
            Operator.GreaterThan :
        "and" == label ?
            Operator.And :
        "or" == label ?
            Operator.Or :
        "add" == label || "multiply" == label || "in" == label ?
            throw new ArgumentException("Invalid operator: " + label)
        :
            throw new NotImplementedException();
}
