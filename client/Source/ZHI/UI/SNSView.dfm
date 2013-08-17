inherited SNSViewer: TSNSViewer
  inherited reText: TRichView
    Options = [rvoAllowSelection, rvoScrollToEnd, rvoShowPageBreaks, rvoAutoCopyText, rvoAutoCopyUnicodeText, rvoAutoCopyRVF, rvoAutoCopyImage, rvoAutoCopyRTF, rvoFormatInvalidate, rvoDblClickSelectsWord, rvoRClickDeselects, rvoFastFormatting]
    OnJump = reTextJump
    ExplicitLeft = 0
    ExplicitTop = 0
  end
end
