<?

// Config
$states_file = 'states.csv';
$states_enum = 'TIDEStates';
$factionsarray = 'FActionsArray';
$fformname = 'TfrmCompelIDE';

$out = array();

// Open file
$fp  = fopen($states_file, "r");


// Read header line
$data = fgetcsv($fp, 1000, ',');

// Extract states names
$states = array();
for ($i=1;$i<count($data);$i++)
  $states[$i-1] = $data[$i];

// Process the actions and their states
$actions = array();
$results = array();
while ( ($data = fgetcsv($fp, 1000, ',')) !== FALSE )
{
  // Parse action
  $action = spliti('[^a-z0-9]', $data[0]);
  if (count($action))
    $action = $action[0];
  else
    $action = $data[0];

  // Store action
  $actions[] = $action;

  // Parse results
  $result = array();
  for ($i=1;$i<count($data);$i++)
    $result[] = strlen($data[$i]) ? 'True' : 'False';

  $result = '(' . join(',', $result) . ')';
  $results[] = $result;
}
fclose($fp);

// Generate states declaration
$out[] = sprintf("const gIDEStates: array[0..%d] of array[%s] of Boolean = (", count($actions)-1, $states_enum);
$c = count($actions);
for ($i=0;$i<$c;$i++)
{
  $t = "\t" . $results[$i];
  if ($i<$c-1)
    $t .= ',';
  $t .= ' // ' . $actions[$i];
  $out[] = $t;
}

$out[] = ");\n";

$out[] = <<<HEREDOC
// $factionsarray: array of TAction;
// procedure ChangeActionsState(AState: $states_enum);
procedure $fformname.ChangeActionsState(AState: $states_enum);
var
  i: Integer;
begin
  if Length($factionsarray) = 0 then
  begin
    SetLength($factionsarray, $c);
HEREDOC;

$cm1 = $c-1;
for ($i=0;$i<$c;$i++)
  $out[] = "    {$factionsarray}[$i] := " . $actions[$i] . ';';
$out[] = "  end;";
$out[] = <<<HEREDOC

  for i := 0 to $cm1 do
    {$factionsarray}[i].Enabled := gIDEStates[i][AState];
end;
HEREDOC;

$out = join("\n", $out);

$fp = fopen('states.txt', 'w');
fwrite($fp, $out);
fclose($fp);

?>