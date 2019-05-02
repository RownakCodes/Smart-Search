Module Module1

    Dim countindex As Integer
    Dim WrongEntry As Boolean




    Sub Main()
        Dim Database As New IO.StreamReader("F:\text.txt")
        Console.WriteLine("                                      Welcome to pre-release part 2 ")
        Console.WriteLine("                                             By Sharfaraz")
        Console.WriteLine("********************************************************************************************************")
        countNumberEntries()
        Dim array(countindex, 3) As String


        dividingArray(array)

        Database.Close()
        Do
            WrongEntry = False
            VerifyID(array)
            Console.Clear()
        Loop Until WrongEntry = False

        Database.Close()

        SearchArray(array)


        Console.Clear()
        Console.WriteLine("Did anyone leave the School? (Enter True if yes or False if no)")
        Dim leave As Boolean
        leave = Console.ReadLine
        If leave = True Then
            leaveSch(array)
        End If

    End Sub

    Sub VerifyID(arr(,) As String)
        VerifyNumPart(arr)
        Console.Clear()
        '-------------------------------------------------------------------------------
        verifyCharPart(arr)
        Console.Clear()
        '--------------------------------------------------------------------------------
        verifyDashPart(arr)

    End Sub


    Sub verifyDashPart(arr(,) As String)
        Dim content, correction As String

        For index = 0 To UBound(arr)
            content = arr(index, 3)
            If Mid(content, 6, 1) <> "-" Then
                Console.WriteLine("There is an error in the ID does not follow the format: ")

                title()

                For column = 0 To 3
                    spacing(arr(index, column))
                Next


                Console.WriteLine()
                Console.WriteLine("Please re-enter the ID of this particular student: ")
                Console.WriteLine()
                correction = Console.ReadLine()


                arr(index, 3) = correction

                WrongEntry = True
            End If

        Next

    End Sub

    Sub verifyCharPart(arr(,) As String)
        Dim count, ChrPart As Integer
        Dim content, correction As String

        For index = 0 To UBound(arr)
            content = arr(index, 3)
            count = 1
            ChrPart = 0
            Do
                If Mid(content, count, 1) >= "A" And Mid(content, count, 1) <= "Z" Then
                    ChrPart = ChrPart + 1
                End If
                count = count + 1
            Loop Until count = Len(content)
            correction = ""
            If ChrPart <> 0 Then

                Console.WriteLine("The Character Part of the ID does not follow the format: ")

                title()

                For column = 0 To 3
                    spacing(arr(index, column))
                Next
                Console.WriteLine()

                Console.WriteLine("Please enter the character part of this particular ID: ")
                Console.WriteLine()
                correction = Console.ReadLine

                content = Left(content, 6) + correction
                arr(index, 3) = content

                WrongEntry = True
            End If
        Next
    End Sub



    Sub VerifyNumPart(arr(,) As String)
        Dim count, NumberOfNumbers As Integer
        Dim content, correction As String

        For index = 0 To UBound(arr)


            content = arr(index, 3)
            NumberOfNumbers = 0
            count = 0

            Do
                If IsNumeric(content(count)) Then
                    NumberOfNumbers = NumberOfNumbers + 1
                End If
                count = count + 1
            Loop Until content(count) = "-" Or count = Len(content) - 1

            If NumberOfNumbers <> 5 Then
                title()
                Console.WriteLine("This particular id contains an error: ")

                For column = 0 To 3
                    spacing(arr(index, column))
                Next


                Console.WriteLine("")
                Console.WriteLine("Please re-enter the ID of this particular student: ")
                correction = Console.ReadLine()

                WrongEntry = True
                correction = correction + Right(content, 2)
                arr(index, 3) = correction

            End If
        Next

    End Sub


    Sub leaveSch(arr(,) As String)

        Dim nameRemove As String = ""
        Dim count As Integer = 0
        printArray(arr)
        While nameRemove <> "xxx"
            Console.WriteLine("Enter the full name of the studen who left: (enter xxx to exit) ")
            count = 0

            nameRemove = Console.ReadLine
            For index = 0 To UBound(arr)
                If nameRemove = arr(index, 0) Then
                    count = count + 1
                    For column = 0 To 3
                        arr(index, column) = "Null"
                    Next
                End If

            Next
            Console.Clear()


            removeNull(arr)
            printArray(arr)

            If count = 0 And nameRemove <> "xxx" Then
                Console.WriteLine("There is no one with the name {0} or his/her data is already removed", nameRemove)
            End If
        End While
    End Sub


    Sub removeNull(arr(,) As String)
        For index = 0 To UBound(arr)
            If arr(index, 0) = "Null" Then
                For removeIndex = index To UBound(arr) - 1
                    For column = 0 To 3
                        arr(removeIndex, column) = arr(removeIndex + 1, column)
                    Next
                Next
            End If
        Next
    End Sub


    Sub title()
        Dim header As String

        Console.WriteLine()
        Console.WriteLine()
        header = ""
        For x = 1 To 4
            Select Case x
                Case 1
                    header = "Name"
                Case 2
                    header = "Email"
                Case 3
                    header = "Date of Birth"
                Case 4
                    header = "Student ID"
            End Select

            Console.Write(header)
            For y = 0 To 30 - Len(header)
                Console.Write(" ")
            Next

        Next

        Console.WriteLine()
        For dash = 0 To 115
            Console.Write("-")
        Next
        Console.WriteLine()
    End Sub

    Sub SearchArray(array(,) As String)
        Console.Clear()
        printArray(array)

        Do
            Console.WriteLine()
            Console.WriteLine()
            Console.WriteLine()

            Console.WriteLine("Enter (the respective number of) the feild you want to search with")

            Console.WriteLine("1. Part of Name")
            Console.WriteLine("2. Email Address")
            Console.WriteLine("3. Student ID")
            Console.WriteLine("4. Show Database")
            Console.WriteLine("5. Exit")

            Dim searchtype As Integer
            Console.WriteLine()
            Console.WriteLine()
            Console.WriteLine()
            searchtype = Console.ReadLine()

            Select Case searchtype

                Case 1
                    experiment(array, 0)
                Case 2
                    experiment(array, 1)
                Case 3
                    experiment(array, 3)
                Case 4
                    Console.Clear()
                    Console.WriteLine("                                                  Database")
                    printArray(array)
                Case 5
                    Exit Do
            End Select

        Loop
    End Sub


    Sub experiment(arr(,) As String, n As Integer)

        Dim inp As ConsoleKeyInfo
        Dim searchString As String = ""
        Console.WriteLine()
        Console.WriteLine()
        Console.WriteLine()
        Dim count As Integer = 0

        Console.WriteLine("Enter the name you want to search: ")

        inp = Console.ReadKey()
        Dim content, sort As String
        searchString = searchString + (inp.KeyChar)

        While inp.KeyChar <> Chr(27)
            Console.Clear()
            title()
            For index = 0 To UBound(arr)
                content = arr(index, n)

                For position = 1 To Len(content) - Len(searchString)
                    sort = Mid(content, position, Len(searchString))
                    If sort = searchString Then
                        count = count + 1
                        For row = 0 To 3
                            spacing(arr(index, row))

                        Next
                        Console.WriteLine()
                    End If

                Next
            Next



            Console.WriteLine()
            Console.WriteLine()
            Console.WriteLine()


            Console.WriteLine("Enter the name you want to search: ")
            Console.Write(searchString)

            inp = Console.ReadKey()
            If inp.KeyChar = Chr(8) Then
                searchString = Left(searchString, Len(searchString) - 1)
            Else
                searchString = searchString + (inp.KeyChar)
            End If

        End While
    End Sub



    Sub countNumberEntries()
        Dim Database As New IO.StreamReader("F:\text.txt")
        countindex = 0
        Dim content As String

        content = Database.ReadLine
        While Not (Database.EndOfStream)
            countindex = countindex + 1
            content = Database.ReadLine
        End While
        Database.Close()
    End Sub


    Sub dividingArray(arr(,) As String)
        Dim Database As New IO.StreamReader("F:\text.txt")
        Dim contentline, name, email, DOB, ID As String
        Dim hashcount, index, count As Integer
        index = 0
        hashcount = 0


        Do
            contentline = Database.ReadLine()
            ID = ""
            DOB = ""
            email = ""
            name = ""
            count = 0

            Do
                name = name + contentline(count)
                count = count + 1
            Loop Until contentline(count) = "#"
            arr(index, 0) = name
            count = count + 1

            Do
                email = email + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = "#"
            arr(index, 1) = email
            count = count + 1

            Do

                DOB = DOB + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = "#"
            arr(index, 2) = DOB
            count = count + 1

            Do
                ID = ID + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = ""
            arr(index, 3) = ID




            index = index + 1
        Loop Until Database.EndOfStream
        Database.Close()

    End Sub


    Sub printArray(arr(,) As String)
        title()
        For row = 0 To UBound(arr)
            For column = 0 To 3
                spacing(arr(row, column))
            Next
            Console.WriteLine("")
        Next
    End Sub



    Sub spacing(value As String)
        'Make a constant space of 30 spaces

        Console.Write(value)
        For x = 0 To 30 - Len(value)
            Console.Write(" ")
        Next

    End Sub

End Module
