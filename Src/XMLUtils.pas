unit XMLUtils;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, xmldom, XMLIntf, msxmldom, XMLDoc, ComCtrls, StdCtrls, ExtCtrls;

procedure XMLtoTreeView(TreeView:TTreeView; XMLNode:IXMLNode; TreeNode:TTreeNode);
function GetAttributeText(Node:IXMLNode; Name:string):string;

implementation

procedure XMLtoTreeView(TreeView:TTreeView; XMLNode:IXMLNode; TreeNode:TTreeNode);
var
  Loop : integer;
begin
  if XMLNode.NodeType <> ntElement then Exit;

  TreeNode:= TreeView.Items.AddChild(TreeNode, XMLNode.NodeName);
  TreeNode.Data:= Pointer(XMLNode);

  for Loop := 0 to XMLNode.ChildNodes.Count - 1 do
    XMLtoTreeView(TreeView, XMLNode.ChildNodes.Nodes[Loop], TreeNode);
end;

function GetAttributeText(Node:IXMLNode; Name:string):string;
var
  XMLNode : IXMLNode;
begin
  Result:= '';
  if Node = nil then Exit;

  XMLNode:= Node.AttributeNodes.FindNode(Name);
  if XMLNode = nil then Exit;

  try
    Result:= XMLNode.Text;
  except
    Result:= '';
  end;
end;

end.
