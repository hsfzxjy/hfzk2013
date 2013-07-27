unit UIThread;

interface

uses
  Classes;

type
  TSyncMethod = procedure(Sender: TObject);

  TUIThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    SyncMethod: TSyncMethod;
  end;

implementation

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TUIThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TUIThread }

procedure TUIThread.Execute;
begin
  { Place thread code here }
  self.StaticQueue();
end;

end.
