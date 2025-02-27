
import React, { useState } from "react"


import {
  GroupByOption,
  GroupingConfig,
} from "./GroupingConfig"
import { TransactionSourceSelector } from "./TransactionSourceSelector"
import { WithdrawDepositSelector } from "./WithdrawDepositSelector"
import  BoxWithText  from "./BoxWithText"
import { type TransactionSourceGroup } from "../../client"

import type { TransactionsGetAggregatedTransactionsResponse } from "../../client"



export function FilterGroup({
  activeTransactionSource,
  setActiveTransactionSource,
  data,
  groupingOptions,
  setGroupingOptions,
}: {
  activeTransactionSource: TransactionSourceGroup,
  setActiveTransactionSource: React.Dispatch<
    React.SetStateAction<TransactionSourceGroup | null>
  >,
  data: TransactionsGetAggregatedTransactionsResponse,
  groupingOptions: GroupByOption[],
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>
}) {
  const [isExpanded, setIsExpanded] = useState(true)
  const [showDeposits, setShowDeposits] = useState(false)

  return (
           <BoxWithText
              text="Filters"
              isExpanded={isExpanded}
              setIsExpanded={setIsExpanded}
            >
              <WithdrawDepositSelector
                setShowDeposits={setShowDeposits}
                showDeposits={showDeposits}
              />
              <TransactionSourceSelector
                activeTransactionSource={activeTransactionSource}
                setActiveTransactionSource={setActiveTransactionSource}
                allTransactionSources={data.groups}
              />
              <GroupingConfig
                groupingOptions={groupingOptions}
                setGroupingOptions={setGroupingOptions}
              />
            </BoxWithText>
  )
  
}
